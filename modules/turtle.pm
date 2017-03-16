ctmc 

// Initial params for inventory
const double init_coal = 10;
const int init_diamond = 0;
const int init_saplings = 0;

// Global variables
global mines_exhausted : int init 0;
global available_tree_farms : int init 1;

// Global costs
const int fixed_travel_cost = 2;
// Mine travel cost gets updated everytime mining occurs
const double mine_travel_factor = 0.05;

// Action costs
const double tree_action_cost = 0.05;
const double mine_action_cost = 0.10;

// Other costs
const double birth_cost = 3;

// Movement rates
const double fixed_move_time = 10;
const mining_move_time_factor : double init 1;

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
    // 2 - tree_farm_farm
    // 3 - tree_farm_build
    // 4 - mine
    // 5 - storage
    // 6 - birth
    // 7 - dead
    s : [0..7] init 0;

    // Non-deterministic birth based on counter
    [birth] s = 0 & birth_counter = 0 -> s' = 1;
    [birth] s = 0 & birth_counter != 0 -> s' = 0;

    // Rule for going to tree farm for construction.
    // WIP
    [] s = 1 & coal >= fixed_travel_cost -> 

    // Inventory
    coal : double init init_coal;
    diamond : int init init_diamond;
    saplings : int init init_saplings;

endmodule
