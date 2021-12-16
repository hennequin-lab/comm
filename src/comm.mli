include module type of Comm_typ

module Single (R : sig
  val init_rng : int -> unit
end) : T

module Mpi (R : sig
  val init_rng : int -> unit
end) : T
