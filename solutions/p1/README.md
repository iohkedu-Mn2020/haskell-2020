# p1 A peer-to-peer network

This is the first project exercise, and you should work on it in your teams.
At the deadline, you either have to
demonstrate the solution or at least provide a status report.

The goal is to implement a simple peer-to-peer (p2p) discovery protocol.

Each team will have its own solution, but the different solutions will all follow
the same protocol and should therefore be able to “talk” to each other.

The protocol is text (and line) based and given by the following datatype of
messages:

```haskell
module P1 where

data Message = Connect HostName PortNumber
             | GetPeers
             | Status [Peer]
             | Newtx Tx
             | Oldtx Tx Tx
             | Quit
             | Unknown String
             deriving (Show, Read)

```

We will learn about proper parsing and serialization to binary formats soon, but
for this task, you can use `read` and `show` for parsing and serialization.

Transactions are just `Int`s here:

```haskell
type Tx = Int
```

and a peer is just a pair of host name and port number:

```haskell
data Peer = Peer HostName PortNumber
    deriving (Show, Read, Eq, Ord)
```

In a p2p setting, every process acts as both a server and a client. We try to build
a network where several processes connect to each other and propagate a bit of
shared state, which is the number of the most recent (maximum) transaction.

The `Connect` message is special: A process has to send this message upon
connecting to another process, and it must not sent it again after that. The
`Connect` message is supposed to be parameterized by the hostname and port
number under which the originating process can be contacted.

The `Quit` message, if sent, should cause the target process to end the connection
properly.

The `GetPeers` message can be used to request a `Status` response, in which a
process lists its known peer processes.

The `Newtx` message can be used to inform another process about a possible new
transaction.

The `Oldtx` message is purely informational and states that a particular
transaction is already known to the process, and it also includes the most
recent transaction.

The `Unknown` message can be used to signal incorrect inputs, and thereby help
debugging erroneous programs.

Every process maintains a list of peers, which are other processes it is connected
to, by remembering at least their host names and port numbers. There are
two ways in which a process can be connected to another process. It may have
initiated the connection itself (after having been informed about a possible peer
via a message), or it received an incoming connection.

A process can decide how many peers it wants to be connected to. I recommend
to implement at least the following strategy: A process will accept and retain
any incoming connection. On startup, a process will be given a possible seed
node as a command line parameter. It will then try to connect to that node and
ask it for its peers. On receiving the peers, it might connect to these and ask
them for their peers as well, and continue this process for a few times (making
sure not to unnecessarily connect to the same process over and over again). After
the process has either no ways of finding new peers anymore, or reached at least
knowledge of a given number of possible peers (e.g. 10), it will randomly pick a
small number (e.g. 3) out of these and connect to them.

This way, a loosely connected network can be built by connecting more and more
nodes and letting them choose what other nodes to connect to.
Processes should also retain the most recent transaction they’ve seen. Whenever
they receive a new transaction, they should propagate it, but with a (configurable)
delay. Propagate means they’ll just re-issue the `Newtx` message to their peers. If
a process receives a transaction that it already knows or is less recent than its
current transaction, it should instead reply with an `Oldtx` message, including
both the transaction it rejected and the maximum transaction it knows about.
A `Newtx 0` message can be used by a new process to figure out what the peers
believe the current transaction is.

Processes should log incoming transactions to a file or the terminal with timestamps.
Look at the `time` package on how you can access the system time.
Processes should, at random intervals (between 10 seconds and a few minutes),
generate new transactions, between 1 and 10 higher than the most recent
transaction they currently know. They should log when they create a new
transaction and then send it to their peers.
Processes should deal with error situations properly. If other processes disappear,
the network might seize functioning, but at the very least processes should
gracefully handle such situations and not crash.

## Optional features

- Every process could also have an interactive interface where you can tell it
  to send particular messages right now.
- If processes disconnect and the number of peers of a node gets below a
  certain level, the node could re-start the discovery process and try to find
  new peers to connect to.
- You could try to let processes map out the complete network graph as
  they perceive it by sending subsequent peer messages and export it as a
  graph. Look at the `DOT` file format of `graphviz` to export a graph in an
  easily drawable format. There are also Hackage `graphviz` packages, but
  they’re probably overkill for this.
- You could try to collect and aggregate the logs of the different nodes and
  visualize how transactions propagate through the network.
- Think of other interesting features you might add.

## Important

- Keep your code clean and refactor often. Readability and clarity is more
  important than having as many features as possible. Documentation also helps.
- Everyone must be able to answer general questions about the code. So
  even though you work together in a team, talk to each other about the code
  you write and review each other’s code, so that you know what they’ve
  written.
