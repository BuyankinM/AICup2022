use rand::Rng;
use std::collections::HashMap;

use crate::debug_interface::DebugInterface;

use ai_cup_22::model::ActionOrder;
use ai_cup_22::model::Item;
use ai_cup_22::model::Loot;
use ai_cup_22::model::Order;
use ai_cup_22::model::Unit;
use ai_cup_22::model::UnitOrder;
use ai_cup_22::model::Vec2;
use ai_cup_22::*;

const K_SPIRAL: f64 = 0.95;
const K_VEC: f64 = 777.0;
const TICKS_TO_RUN: u8 = 5;
const MAX_OBS_RADIUS: f64 = 3.0;
const NUM_UNITS: usize = 2;

pub trait Vec2Ops {
    fn zero() -> Self;
    fn len(&self) -> f64;
}

impl Vec2Ops for Vec2 {
    fn zero() -> Self {
        Vec2 { x: 0.0, y: 0.0 }
    }

    fn len(&self) -> f64 {
        (self.x.powi(2) + self.y.powi(2)).sqrt()
    }
}

#[derive(Clone)]
enum State {
    Walk,
    GoToItem {
        id: i32,
        pos: Vec2,
    },
    GetItem(i32),
    UseShield,
    Attack {
        pos: Vec2,
        dir: Vec2,
        spawn_time: f64,
    },
    RunAway {
        dir: Vec2,
    },
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
enum OperationType {
    GoToShield = 0b1,
    GoToWeapon = 0b10,
    GoToAmmo = 0b100,
    UseShield = 0b1000,
    Attack = 0b10000,
    RunAway = 0b100000,
}

pub struct MyStrategy {
    state: State,
    operations: HashMap<OperationType, State>,
    operations_bin: u32,
    ticks_to_run: HashMap<i32, u8>,
    constants: model::Constants,
    my_id: i32,
    my_dir: Vec2,
    my_pos: Vec2,
    my_health: f64,
    my_shield: f64,
    my_shield_potions: i32,
    my_weapon: i32,
    my_weapon_num: i32,
    my_ammo: Vec<i32>,
    my_remaining_spawn_time: f64,
    zone_center: Vec2,
    zone_radius_2: f64,
}

impl MyStrategy {
    pub fn new(constants: model::Constants) -> Self {
        Self {
            state: State::Walk,
            operations: HashMap::new(),
            operations_bin: 0,
            ticks_to_run: HashMap::with_capacity(NUM_UNITS),
            constants,
            my_id: 0,
            my_dir: Vec2::zero(),
            my_pos: Vec2::zero(),
            my_health: 0.0,
            my_shield: 0.0,
            my_shield_potions: 0,
            my_weapon: 0,
            my_weapon_num: 0,
            my_ammo: Vec::new(),
            my_remaining_spawn_time: 0.0,
            zone_center: Vec2::zero(),
            zone_radius_2: 0.0,
        }
    }

    pub fn get_order(
        &mut self,
        game: &model::Game,
        _debug_interface: Option<&mut DebugInterface>,
    ) -> model::Order {
        let mut unit_orders = HashMap::with_capacity(NUM_UNITS);

        let (my_units, enemies): (Vec<_>, Vec<_>) = game
            .units
            .iter()
            .partition(|unit| unit.player_id == game.my_id);

        self.zone_center = game.zone.current_center.clone();
        self.zone_radius_2 = (game.zone.current_radius - self.constants.unit_radius).powi(2);

        for my_unit in &my_units {
            self.set_deafult_state();

            self.my_id = my_unit.id;
            self.my_pos = my_unit.position.clone();
            self.my_dir = my_unit.direction.clone();
            self.my_health = my_unit.health;
            self.my_shield = my_unit.shield;
            self.my_shield_potions = my_unit.shield_potions;
            self.my_ammo = my_unit.ammo.clone();
            self.my_remaining_spawn_time = my_unit.remaining_spawn_time.unwrap_or(0.0);

            (self.my_weapon, self.my_weapon_num) = match my_unit.weapon {
                Some(id) => (id, my_unit.ammo[id as usize]),
                None => (-1, 0),
            };

            let sounds_of_attack = game
                .sounds
                .iter()
                .filter_map(|s| {
                    if dist_euclid_square(&s.position, &self.my_pos) <= self.constants.unit_radius {
                        Some((s.type_index, &s.position))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            let safe_way = self.prepare_safe_way(&enemies, &sounds_of_attack);

            self.check_redzone();
            self.check_spawn();
            self.check_danger(&enemies);

            if !self.is_run_away() {
                self.check_shield();
                self.check_attack(&enemies);
                self.walk_and_loot(game);
                self.go_and_take_loot();
            }

            let (mut target_velocity, target_direction, action) = match self.state {
                State::Walk => (safe_way.clone(), self.default_rotate_direction(30.0), None),

                State::GoToItem { ref pos, .. } => {
                    let vec_to_target = sub_vec(pos, &self.my_pos);
                    (vec_to_target.clone(), vec_to_target, None)
                }

                State::GetItem(loot) => {
                    self.set_deafult_state();
                    (
                        safe_way.clone(),
                        self.my_dir.clone(),
                        Some(ActionOrder::Pickup { loot }),
                    )
                }

                State::UseShield => {
                    self.set_deafult_state();
                    (
                        safe_way.clone(),
                        self.default_rotate_direction(30.0),
                        Some(ActionOrder::UseShieldPotion {}),
                    )
                }

                State::Attack {
                    ref pos,
                    ref dir,
                    spawn_time,
                } => {
                    let vec_to_target = sub_vec(pos, &self.my_pos);
                    let cos_view = cos_vec(&vec_to_target, dir);
                    let velocity = match cos_view {
                        _ if cos_view <= -0.7 => self.spiral_rotate_direction(pos, 60.0, 0.2),
                        _ if cos_view >= 0.5 => vec_to_target.clone(),
                        _ => {
                            let v1 = rotate_vec(&vec_to_target, 90.0, None);
                            let v2 = rotate_vec(&vec_to_target, -90.0, None);
                            let p = match cos_vec(&v1, dir) > cos_vec(&v2, dir) {
                                true => 1.0,
                                false => 0.0,
                            };
                            self.spiral_rotate_direction(pos, 60.0, p)
                        }
                    };

                    let is_visible = self.check_enemy_visibility(pos);
                    let is_acessible = self.check_enemy_accesibility(pos) && spawn_time <= 0.5;
                    let shoot = is_visible && is_acessible;

                    (velocity, vec_to_target, Some(ActionOrder::Aim { shoot }))
                }

                State::RunAway { ref dir } => (dir.clone(), dir.clone(), None),
            };

            target_velocity = self.correct_velocity_near_obstacle(target_velocity);

            let ticks_to_run = self.ticks_to_run.entry(self.my_id).or_default();
            if *ticks_to_run > 0 {
                *ticks_to_run -= 1;
            }

            unit_orders.insert(
                my_unit.id,
                UnitOrder {
                    target_velocity,
                    target_direction,
                    action,
                },
            );
        }

        Order { unit_orders }
    }

    fn set_deafult_state(&mut self) {
        if *self.ticks_to_run.entry(self.my_id).or_default() > 0 {
            return;
        }

        self.state = State::Walk;
        self.operations_bin = 0;
        self.operations.clear();
    }

    fn set_operation_bit(&mut self, op: OperationType) {
        self.operations_bin |= op as u32;
    }

    fn check_operation_bit(&self, op: OperationType) -> bool {
        self.operations_bin & (op as u32) != 0
    }

    fn default_rotate_direction(&self, angle: f64) -> Vec2 {
        rotate_vec(&self.my_dir, angle, None)
    }

    fn spiral_rotate_direction(&self, pos: &Vec2, angle: f64, p: f64) -> Vec2 {
        let vec_enemy_to_me = Vec2 {
            x: self.my_pos.x - pos.x,
            y: self.my_pos.y - pos.y,
        };

        let mut rng = rand::thread_rng();
        let sign = match rng.gen_bool(p) {
            true => 1.0,
            false => -1.0,
        };

        let new_vec = rotate_vec(&vec_enemy_to_me, angle * sign, Some(K_SPIRAL));
        sub_vec(&new_vec, &vec_enemy_to_me)
    }

    fn default_velocity(&self) -> Vec2 {
        Vec2 {
            x: (self.zone_center.x - self.my_pos.x) * K_VEC,
            y: (self.zone_center.y - self.my_pos.y) * K_VEC,
        }
    }

    fn check_shield(&mut self) {
        if (self.constants.max_shield - self.my_shield) < self.constants.shield_per_potion
            || self.my_shield_potions == 0
        {
            return;
        }

        let op = OperationType::UseShield;
        self.set_operation_bit(op);
        self.operations.insert(op, self.state.clone());

        self.state = State::UseShield;
    }

    fn walk_and_loot(&mut self, game: &model::Game) {
        if self.operations_bin != 0 {
            return;
        }

        let mut shields = Vec::new();
        let mut weapons = Vec::new();
        let mut ammo = Vec::new();

        for loot in game
            .loot
            .iter()
            .filter(|loot| self.is_pos_in_zone(&loot.position))
        {
            match loot.item {
                Item::ShieldPotions { amount } => shields.push((loot, amount)),
                Item::Weapon { type_index } => {
                    let idx = type_index as usize;
                    if (type_index > self.my_weapon || self.my_weapon_num == 0)
                        && self.my_ammo[idx] > 0
                    {
                        weapons.push((loot, type_index))
                    }
                }
                Item::Ammo {
                    weapon_type_index,
                    amount,
                } => {
                    let idx = weapon_type_index as usize;
                    if self.my_ammo[idx] < self.constants.weapons[idx].max_inventory_ammo {
                        let find_first = (self.my_weapon == weapon_type_index
                            && weapon_type_index >= 1)
                            || weapon_type_index != self.my_weapon
                            || self.my_weapon_num == 0;
                        ammo.push((loot, find_first, amount))
                    }
                }
            }
        }
        self.choose_shields(&shields);
        self.choose_weapon(&weapons);
        self.choose_ammo(&ammo);
    }

    fn go_and_take_loot(&mut self) {
        if self.operations_bin & 0b111 == 0 {
            return;
        }

        let check_op = [
            OperationType::GoToShield,
            OperationType::GoToWeapon,
            OperationType::GoToAmmo,
        ];

        for op in &check_op {
            if let Some(state) = self.operations.get(op) {
                self.state = state.clone();
                break;
            }
        }

        if let State::GoToItem { id, pos } = &self.state {
            if dist_manh(&self.my_pos, pos) <= self.constants.unit_radius {
                self.state = State::GetItem(*id);
            }
        }
    }

    fn check_attack(&mut self, enemies: &Vec<&Unit>) {
        let op = OperationType::Attack;

        if enemies.is_empty()
            || self.my_shield == 0.0
            || self.my_weapon == -1
            || self.my_weapon_num == 0
        {
            if self.check_operation_bit(op) {
                // drop previous attack
                self.set_deafult_state();
            }
            return;
        }

        let _full_equipment = self.is_full_equipment();
        if let Some((enemy, _)) = enemies
            .iter()
            .filter(|e| (self.check_enemy_accesibility(&e.position)))
            .map(|e| (e, dist_manh(&self.my_pos, &e.position)))
            .min_by(|a, b| a.1.total_cmp(&b.1))
        {
            self.state = State::Attack {
                pos: enemy.position.clone(),
                dir: enemy.direction.clone(),
                spawn_time: enemy.remaining_spawn_time.unwrap_or(0.0),
            };

            self.set_operation_bit(op);
            self.operations.insert(op, self.state.clone());
        }
    }

    fn choose_shields(&mut self, shields: &Vec<(&Loot, i32)>) {
        if shields.is_empty()
            || self.my_shield_potions == self.constants.max_shield_potions_in_inventory
        {
            return;
        }

        let (loot, _, _) = shields
            .iter()
            .map(|(loot, amount)| (loot, amount, dist_manh(&loot.position, &self.my_pos)))
            .min_by(|a, b| a.2.total_cmp(&b.2))
            .unwrap();

        let op = OperationType::GoToShield;
        self.set_operation_bit(op);
        self.operations.insert(
            op,
            State::GoToItem {
                id: loot.id,
                pos: loot.position.clone(),
            },
        );
    }

    fn choose_weapon(&mut self, weapons: &Vec<(&Loot, i32)>) {
        if weapons.is_empty() {
            return;
        }

        let (loot, _, _) = weapons
            .iter()
            .map(|(loot, type_index)| (loot, type_index, dist_manh(&loot.position, &self.my_pos)))
            .min_by(|a, b| b.1.cmp(a.1).then(a.2.total_cmp(&b.2)))
            .unwrap();

        let op = OperationType::GoToWeapon;
        self.set_operation_bit(op);
        self.operations.insert(
            op,
            State::GoToItem {
                id: loot.id,
                pos: loot.position.clone(),
            },
        );
    }

    fn choose_ammo(&mut self, ammo: &Vec<(&Loot, bool, i32)>) {
        if ammo.is_empty() {
            return;
        }

        let (loot, _, _, _) = ammo
            .iter()
            .map(|(loot, find_first, amount)| {
                (
                    loot,
                    find_first,
                    amount,
                    dist_manh(&loot.position, &self.my_pos),
                )
            })
            .min_by(|a, b| b.1.cmp(a.1).then(a.3.total_cmp(&b.3)))
            .unwrap();

        let op = OperationType::GoToAmmo;
        self.set_operation_bit(op);
        self.operations.insert(
            op,
            State::GoToItem {
                id: loot.id,
                pos: loot.position.clone(),
            },
        );
    }

    fn check_enemy_visibility(&self, pos: &Vec2) -> bool {
        let (min_x, max_x) = match pos.x < self.my_pos.x {
            true => (pos.x, self.my_pos.x),
            false => (self.my_pos.x, pos.x),
        };
        let (min_y, max_y) = match pos.y < self.my_pos.y {
            true => (pos.y, self.my_pos.y),
            false => (self.my_pos.y, pos.y),
        };

        let obstacles = self
            .constants
            .obstacles
            .iter()
            .filter_map(|ob| {
                if ob.position.x >= min_x - ob.radius
                    && ob.position.x <= max_x + ob.radius
                    && ob.position.y >= min_y - ob.radius
                    && ob.position.y <= max_y + ob.radius
                    && !ob.can_shoot_through
                {
                    Some((ob.position.x, ob.position.y, ob.radius))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        if obstacles.is_empty() {
            return true;
        }

        // ax + by + c = 0
        let a = (pos.y - self.my_pos.y) / (pos.x - self.my_pos.x);
        let b = -1.0_f64;
        let c = pos.y - a * pos.x;

        let denominator = (a.powi(2) + b.powi(2)).sqrt();
        obstacles.into_iter().all(|(x0, y0, radius)| {
            let dist = (a * x0 + b * y0 + c).abs() / denominator;
            dist > radius
        })
    }

    fn check_enemy_accesibility(&self, pos: &Vec2) -> bool {
        self.get_weapon_dist(self.my_weapon) >= dist_euclid(&self.my_pos, pos)
    }

    fn get_weapon_dist(&self, weapon_idx: i32) -> f64 {
        let weapon = &self.constants.weapons[weapon_idx as usize];
        weapon.projectile_speed * weapon.projectile_life_time
    }

    fn check_danger(&mut self, enemies: &Vec<&Unit>) {
        if enemies.is_empty() || self.is_run_away() {
            return;
        }

        for en in enemies {
            let cos_to_me = self.get_enemy_cos_to_me(en);
            let en_power = (en.health + en.shield) / (self.my_health + self.my_shield);

            if let Some(idx) = en.weapon {
                let dist_to_me = dist_euclid(&self.my_pos, &en.position);
                let weapon_dist = self.get_weapon_dist(idx);
                let can_shoot = weapon_dist >= dist_to_me;

                if can_shoot
                    && (idx > self.my_weapon || en_power > 1.5)
                    && en.ammo[idx as usize] > 0
                    && cos_to_me > 0.9
                    && en.aim >= 0.5
                {
                    self.state = State::RunAway {
                        dir: self.default_rotate_direction(90.0),
                    };

                    let op = OperationType::RunAway;
                    self.set_operation_bit(op);
                    self.operations.insert(op, self.state.clone());

                    self.ticks_to_run.insert(self.my_id, TICKS_TO_RUN);
                    break;
                }
            }
        }
    }

    fn check_redzone(&mut self) {
        if self.is_pos_in_zone(&self.my_pos) {
            return;
        }

        self.state = State::RunAway {
            dir: self.default_velocity(),
        };

        let op = OperationType::RunAway;
        self.set_operation_bit(op);
        self.operations.insert(op, self.state.clone());

        self.ticks_to_run.insert(self.my_id, TICKS_TO_RUN);
    }

    fn check_spawn(&mut self) {
        if self.my_remaining_spawn_time == 0.0 {
            return;
        }

        self.state = State::RunAway {
            dir: self.default_velocity(),
        };

        let op = OperationType::RunAway;
        self.set_operation_bit(op);
        self.operations.insert(op, self.state.clone());
    }

    fn correct_velocity_near_obstacle(&self, target_velocity: Vec2) -> Vec2 {
        let delta = self.constants.unit_radius + MAX_OBS_RADIUS;
        let (min_x, max_x) = (self.my_pos.x - delta, self.my_pos.x + delta);
        let (min_y, max_y) = (self.my_pos.y - delta, self.my_pos.y + delta);

        let near_obstacle = self
            .constants
            .obstacles
            .iter()
            .filter_map(|ob| {
                let vec_to_obs = sub_vec(&ob.position, &self.my_pos);
                let dist_to_obs = dist_euclid(&ob.position, &self.my_pos);
                let radius_sum = self.constants.unit_radius + ob.radius;

                if ob.position.x >= min_x
                    && ob.position.x <= max_x
                    && ob.position.y >= min_y
                    && ob.position.y <= max_y
                    && cos_vec(&vec_to_obs, &target_velocity) > 0.0
                {
                    let mut delta_dist = 2.0;
                    if let State::GoToItem { ref pos, .. } = self.state {
                        let dist_obs_to_item = dist_euclid(&ob.position, pos);
                        if dist_obs_to_item <= ob.radius + delta_dist {
                            // we need go closer
                            delta_dist = 0.0;
                        }
                    }
                    match dist_to_obs <= radius_sum + delta_dist {
                        true => Some(vec_to_obs),
                        false => None,
                    }
                } else {
                    None
                }
            })
            .next();

        if let Some(vec_to_obs) = near_obstacle {
            let v1 = rotate_vec(&vec_to_obs, 120.0, None);
            let v2 = rotate_vec(&vec_to_obs, -120.0, None);
            match cos_vec(&v1, &target_velocity) > cos_vec(&v2, &target_velocity) {
                true => v1,
                false => v2,
            }
        } else {
            target_velocity
        }
    }

    fn correct_velocity_by_attack(&self, sounds: Vec<(i32, &Vec2)>) -> Vec2 {
        let (_, pos_sound) = sounds.into_iter().max_by_key(|(id, _)| *id).unwrap();
        let vec_of_attack = sub_vec(pos_sound, &self.my_pos);
        rotate_vec(&vec_of_attack, -120.0, None)
    }

    fn prepare_safe_way(&self, enemies: &Vec<&Unit>, sounds_of_attack: &Vec<(i32, &Vec2)>) -> Vec2 {
        let mut safe_way = Vec2::zero();

        enemies.iter().for_each(|enemy| {
            let dir_to_me = sub_vec(&self.my_pos, &enemy.position);
            safe_way.x += dir_to_me.x;
            safe_way.y += dir_to_me.y;
        });

        sounds_of_attack.iter().for_each(|(_, pos)| {
            let dir_to_me = sub_vec(&self.my_pos, pos);
            safe_way.x += dir_to_me.x;
            safe_way.y += dir_to_me.y;
        });

        match enemies.is_empty() && sounds_of_attack.is_empty() {
            true => self.default_velocity(),
            false => rotate_vec(&safe_way, 45.0, None),
        }
    }

    fn get_enemy_cos_to_me(&self, enemy: &Unit) -> f64 {
        let enemy_dir = &enemy.direction;
        let dir_to_me = &sub_vec(&self.my_pos, &enemy.position);
        cos_vec(enemy_dir, dir_to_me)
    }

    fn is_pos_in_zone(&self, pos: &Vec2) -> bool {
        dist_euclid_square(pos, &self.zone_center) < self.zone_radius_2
    }

    fn is_full_equipment(&self) -> bool {
        self.my_shield == self.constants.max_shield
            && self.my_shield_potions == self.constants.max_shield_potions_in_inventory
            && self
                .my_ammo
                .iter()
                .zip(self.constants.weapons.iter())
                .all(|(&my_num, weapon)| my_num == weapon.max_inventory_ammo)
    }

    fn is_run_away(&self) -> bool {
        self.check_operation_bit(OperationType::RunAway)
    }

    pub fn debug_update(&mut self, _displayed_tick: i32, _debug_interface: &mut DebugInterface) {}

    pub fn finish(&mut self) {}
}

fn dist_manh(pos_1: &Vec2, pos_2: &Vec2) -> f64 {
    (pos_1.x - pos_2.x).abs() + (pos_1.y - pos_2.y).abs()
}

fn dist_euclid(pos_1: &Vec2, pos_2: &Vec2) -> f64 {
    ((pos_1.x - pos_2.x).powi(2) + (pos_1.y - pos_2.y).powi(2)).sqrt()
}

fn dist_euclid_square(pos_1: &Vec2, pos_2: &Vec2) -> f64 {
    (pos_1.x - pos_2.x).powi(2) + (pos_1.y - pos_2.y).powi(2)
}

fn sub_vec(pos_1: &Vec2, pos_2: &Vec2) -> Vec2 {
    Vec2 {
        x: (pos_1.x - pos_2.x) * K_VEC,
        y: (pos_1.y - pos_2.y) * K_VEC,
    }
}

fn cos_vec(pos_1: &Vec2, pos_2: &Vec2) -> f64 {
    (pos_1.x * pos_2.x + pos_1.y * pos_2.y) / (pos_1.len() * pos_2.len())
}

fn rotate_vec(v: &Vec2, angle: f64, k: Option<f64>) -> Vec2 {
    let (sin_angle, cos_angle) = (angle.to_radians().sin(), angle.to_radians().cos());
    let k = k.unwrap_or(K_VEC);
    Vec2 {
        x: k * (v.x * cos_angle - v.y * sin_angle),
        y: k * (v.x * sin_angle + v.y * cos_angle),
    }
}
