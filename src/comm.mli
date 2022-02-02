include module type of Comm_typ

module Mpi (R : sig
  val init_rng : int -> unit
end) : T
