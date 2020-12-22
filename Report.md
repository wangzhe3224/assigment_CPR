# Theoretical Parts

1. What changes do you need to make on your code base so it can execute across
   a cluster of nodes?

If we want to achieve fault-tolerance by adding backup nodes, there should be
minor code change to make this project an distributed OTP application by
creating `*.app` file and configuration files to describe master and slave
nodes configuration. Then we can run `erl` with `-sname`  and configuration
path. In this way, if the node running a application goes down, the application
will be restarted at another node.

2. What are the advantages of immutable concurrency models with no shared state
   over those with mutable shared state?

Immutable data reduces floating parts that could go wrong especially in a
concurrent system. No shared state among processes further reduce the risk of
accidentally modify other process's state (plus that state is not allowed to be
modified). This creates a good encapsulation. With immutable data and no shared
state, we can then focus on making better control of states that do need to be
dynamic and mutable. For example, we can design good lock for dynamic states.
While we don't need to set locks to just avoid mutate wrong state accidentally.

3. How will your system scale on multi-core architectures?

It will scale almost linearly across all the cores by the VM. The processes can
be scheduled across different cores. And each scheduler runs in 1 OS thread on
1 core, hence the system get parallelism.

4. When implementing soft real-time systems, what are the advantages of having
   per process garbage collection?

- First, per process garbage collection, GC, runs faster because it cares only
  one process. 
- Secondly, per process GC would not affect other processes or
  "stop the world", hence the system has a more predictable and stable run 
  time performance.
- Lastly, per process GC in concept should be easier to implement hence less
  chance of bugs.
