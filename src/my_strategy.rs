use std::collections::HashMap;

use crate::debug_interface::DebugInterface;

use ai_cup_22::model::ActionOrder;
use ai_cup_22::model::Order;
use ai_cup_22::model::UnitOrder;
use ai_cup_22::model::Vec2;
use ai_cup_22::*;

pub struct MyStrategy {}

impl MyStrategy {
    pub fn new(_constants: model::Constants) -> Self {
        Self {}
    }

    pub fn get_order(
        &mut self,
        game: &model::Game,
        _debug_interface: Option<&mut DebugInterface>,
    ) -> model::Order {
        let my_unit = game
            .units
            .iter()
            .find(|unit| unit.player_id == game.my_id)
            .unwrap();
        let target_velocity = Vec2 {
            x: -my_unit.position.x,
            y: -my_unit.position.y,
        };
        let target_direction = Vec2 {
            x: -my_unit.direction.x,
            y: -my_unit.direction.y,
        };
        let action = Some(ActionOrder::Aim { shoot: true });

        let mut unit_orders = HashMap::with_capacity(1);
        unit_orders.insert(
            my_unit.id,
            UnitOrder {
                target_velocity,
                target_direction,
                action,
            },
        );
        Order { unit_orders }
    }

    pub fn debug_update(&mut self, _debug_interface: &mut DebugInterface) {}

    pub fn finish(&mut self) {}
}
