module type T = sig
  (** number of nodes in the network *)
  val n_nodes : int

  (** wait until all nodes have called [barrier] *)
  val barrier : unit -> unit
 
 (** scatter data from root node *)
  val scatter : 'a array -> 'a
 
  (** gather data onto root node; non-root nodes receive [[| |]] *)
  val gather : 'a -> 'a array

  (* gather data from all nodes to all nodes *)
  val allgather : 'a -> 'a array

  (** same as gather, except that each node [k] sends a ['a option array] [xk].
    The result is seen only at root node, and is a ['a array] [x] such
    that [x.(i) = aki] iif [xk.(i) = Some aki].
    In other words, all nodes form a partition of some array and transmit
    only their share. The partition should be complete.
    Non-root nodes receive [[| |]] *)
  val gatheroption : 'a option array -> 'a array

  (** same as gatheroption, except that the result is given to all nodes *)
  val allgatheroption : 'a option array -> 'a array

  (* broadcast a value to all the nodes, from root;
   the argument is not significant at any of the non-root nodes *)
  val broadcast : 'a -> 'a

  (* broadcast the of f () computed by the root node only *)
  val broadcast' : (unit -> 'a) -> 'a

  (** [root_receive x src]: the root node receives a value from [src] *)
  val root_receive : 'a -> int -> 'a

  (** send a value to root node *)
  val send_to_root : 'a -> unit

  (** node ID *)
  val rank : int

  (** whether I am first or not *)
  val first : bool

  (** restricts some instructions to the root node *)
  val root_perform : (unit -> unit) -> unit

  (** initializes the random number generator, and guarantees that each node
    has a different seed *)
  val self_init_rng : unit -> unit

  (** perform a computation that involves some random number generation
    with the same seed at each node... the result is thus guaranteed
    to be the same everywhere *)
  val with_same_rng : (unit -> 'a) -> 'a

  (** print a string on standard output. Only root node should print. *)
  val print : string -> unit

  (** same as print, with an EOL at the end *)
  val print_endline : string -> unit
end

