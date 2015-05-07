Riak Sets - a Riak based code base for handling sets
======================================


## Building

To build this application type `make release` that will clean
everything, then pull in any rebar dependencies, compile everything an
build an erlang release. The release will be in the `rel` directory.

A node can be started and stopped with the script
`bin/rel/riak_sets`. 

## Adding and removing nodes

To add a node to the ring, first ensure that all nodes have the same
cookie, and can connect.

First start the node with `riak_sets start`.

Then add a new node with the script `rel/bin/riak_sets-admin` this is
a several step process.

First you will use a join command to tell the system that you want to
join a node (or several) to the ring. First do a join, you need to
provide it with an existing cluster node here.  Then you should review
the plan with the command `riak_sets-admin cluster plan`, it will
print out a plan for you to review. If it looks corerct do
`riak_sets-admin cluster commit`

```bash
% riak_sets-admin cluster join <<NODE>>
% riak_sets-admin cluster plan
% riak_sets-admin cluster commit
```

If the `riak_sets-admin` script can also remove nodes, again you will
need to view the plan and then commit.

## Upgrading

Don't upgrade nodes in production, add new nodes to the ring with the
new code, and then remove the old nodes. Ensure that riak has enough
time to move all data off of old nodes before removing any from the cluster

## 
