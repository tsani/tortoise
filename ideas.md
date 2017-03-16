Tortoise
========

Goal: want to discover "very good" plans for maximizing population size.

Strategy:

  * Write a program to generate a plan, convert the plan to a PRISM model
  * Use PRISM to evaluate the plan.

How is PRISM doing this? PRISM is essentially our evaluation function.

What is an agent?
=================

An agent has an inventory consisting of a fixed set of types of items. Each
type has an associated count, which is unbounded.

  * Coal is consumed for movement actions.
  * Diamonds are consumed when constructing new agents.
  * Saplings are consumed during tree farming.

Changing population size
------------------------

For modelling a changing population size, we decide a (large) cap on the
population, say 500 agents, and model agents that aren't born yet as being in a
_not alive yet_ state. When a living agent performs a birthing action, it
causes the net not yet alive agent to transition to the idle state.

Resource task
=============

A _resource task_ (mining or tree farming) produces resources. Moving into and
out of the state of a resource task costs a relatively substantial amount of
fuel. Remaining in that state (by looping) costs a small amount of fuel and
generates a random amount of resources, depending on the task type.

Tree farming
------------

### Building tree farms

Tree farms are _structures_, which agents must construct. In any farm, at most
one agent may be in it at any time.
To construct a tree farm, a certain number of saplings must be spent.

### Using tree farms

Moving into the tree farm state requires that there be an available tree farm.
Once in the state, coal may be spend to farm trees, which produces some amount
of coal and some amount of saplings. The amount of coal produced should exceed
the amount of saplings produced by a significant amount.

Mining
------

Moving into the mining state costs a _parametric_ amount of fuel, which depends
on how much mining has been done.
This captures the idea that mining is a non-renewable resource: once a chunk
has been mined out, it cannot be mined again. Hence, agents must travel farther
away from the base in order to keep mining.

Every time the mining action is taken, a _mining point_ is gained. More mining
points means it costs more to move into the mining state.

What is a plan?
===============

Activation thresholds for the major actions: tree farming and mining. If turtle
resources are below these values, the actions become disabled for that turtle.

Activation thresholds can change over the course of time, as the population
evolves. So rather than find 2 or 3 thresholds, we are actually finding _n_
times 2 or 3, one set of thresholds for each population size.

Trivially, we can make the thresholds fairly high, requiring a lot of spare
resources to perform things. This is a very safe way of doing things. However,
a lot of resources need to be acquired to do this, so the birthing time for a
new agent becomes higher. We want to minimize the amount of time it takes to
max out the population (or just reach the next number of individuals) so it
makes sense to "competitively" minimize both the time-to-next-birth and the
thresholds.

Miscellaneous Ideas
===================

Have another transition system generator for path-finding.
The idea is that each location is a "critical section," and
we want to calculate the expected number of deadlocks
in any "traversal."
