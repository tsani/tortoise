pta 

// Initial params for inventory
const double init_coal = 10;
const int init_diamond = 0;
const int init_saplings = 0;

// Global variables
global mines_exhausted : int init 0;
global available_tree_farms : int init 1;

// Global costs
const int fixed_travel_cost = 2;

// Action costs
const double tree_action_cost = 0.05;
const double mine_action_cost = 0.10;

// Other costs
const double birth_cost = 3;

// Timing
const fixed_travel_time = 10; // in seconds
// Mine travel time gets updated everytime mining occurs
const double mine_travel_time_penalty = 1;
// Mine travel fuel cost gets updated everytime mining occurs
const double mine_travel_cost_penalty = 1;

// Action rates
const double tree_build_rate = 1;
const double tree_farm_rate = 60;
const double turtle_birth_time = 3;
const double storage_time = 0;
const double mine_time = 5;

// Population shit
const int pop_init_size = 10;
global birth_counter : int init 0;

module unborn_turtle
    // States:
    // 0 - unborn
    // 1 - init
    // 2 - tree_farm_farm_transit
    // 3 - tree_farm_farm
    // 4 - tree_farm_build_transit
    // 5 - tree_farm_build
    // 6 - mine_transit
    // 7 - mine
    // 8 - storage_transit
    // 9 - storage
    // 10 - birth_transit
    // 11 - birth
    // 12 - dead
    // 13 - fixed_return_transit
    // 14 - mine_return_transit

    s : [0..14] init 0;
    
    // ***************
    // TRAVEL COMMANDS
    // ***************

    // Non-deterministic birth based on counter
    [birth] s = 0 & birth_counter = 0 -> s' = 1;

    // Dummy birth arrow to satisfy synchronization rules.
    [birth] s = 0 & birth_counter != 0 -> s' = 0;

    // Rule for going to tree farm for farming.
    [] s = 1 & coal >= fixed_travel_cost -> (s' = 2) & (travel_clock' = 0);

    // Transit time for tree farm farming.
    [] s = 2 & travel_clock >= fixed_travel_time -> s' = 3;

    // Rule for going to tree farm for construction.
    [] s = 1 & coal >= fixed_travel_cost -> (s' = 4) & (travel_clock' = 0);

    // Transit time for tree farm construction.
    [] s = 4 & travel_clock >= fixed_travel_time -> (s' = 5);

    // Rule for going to storage
    [] s = 1 & coal >= fixed_travel_cost -> (s' = 8) & (travel_clock' = 0);

    // Transit time going to storage.
    [] s = 8 & travel_clock >= fixed_travel_time -> s' = 9;

    // Rule for going mining
    [] s = 1 & coal >= (fixed_travel_cost + (mines_exhausted * mine_travel_cost_penalty)) -> (s' = 6) & (travel_clock' = 0);

    // Variable transit time for going to mine.
    [] s = 5 & travel_clock >= (fixed_travel_time + (mines_exhausted * mine_travel_time_penalty))
        -> (s' = 7) & (travel_clock' = 0);

    // Rule for going to birth another turtle
    [] s = 1 & coal >= fixed_travel_cost + init_coal & diamond >= birth_cost -> (s' = 10) & (travel_clock' = 0);
    // Travel time for turtle birthing.
    [] s = 10 & travel_clock >= fixed_travel_time
        -> (s' = 11);
    // Corresponding returning rules

    // Rule for death.
    // - Cannot be in storage, nor can be on way to storage (because that's when
    // resources are expended.
    [] s != 8 & s != 9 & coal < min_cost -> s' = 12;

    // Returning to init from tree build,
    // tree farm, storage, or birth.
    [] (s = 3 | s = 5 | s = 9 | s = 11) & coal >= fixed_travel_cost -> (s' = 13) & (travel_clock' = 0);
    [] s = 13 & travel_clock >= fixed_travel_time -> s' = 1;

    // Mine to mine_return_transit
    [] s = 7 & coal >= (fixed_travel_cost + (mines_exhausted * mine_travel_cost_penalty))
        -> (travel_clock' = 0) & (s' = 14);
    // Mine_transit to init
    [] s = 14 & travel_clock' >= (fixed_travel_time + (mines_exhausted * mine_travel_time_penalty)) -> s' = 1;
    

    // ***************
    // ACTION COMMANDS
    // ***************

    // Build another tree farm.
    [] s = 3 & saplings >= 1 -> available_tree_farms' = available_tree_farms + 1 & 

    // Inventory
    coal : double init init_coal;
    diamond : int init init_diamond;
    saplings : int init init_saplings;

endmodule
