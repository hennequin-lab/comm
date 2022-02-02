include Comm_typ

(** {1 Single node} *)

(** Communication on a single node -- trivial, but can be used as a fallback *)
module Mpi (R : sig
  val init_rng : int -> unit
end) =
struct
  let n_nodes = 1
  let barrier () = ()
  let scatter x = x.(0)
  let gather x = [| x |]

  let gatheroption x =
    Array.map
      (function
        | None -> failwith "gatheroption"
        | Some z -> z)
      x


  let allgather x = [| x |]

  let allgatheroption x =
    Array.map
      (function
        | None -> failwith "allgatheroption"
        | Some z -> z)
      x


  let reduce_sum_int x = x
  let reduce_sum_float x = x
  let reduce_sum_bigarray x dst = Bigarray.Genarray.blit x dst
  let broadcast x = x
  let broadcast' f = f ()
  let root_receive x (_ : int) = x
  let send_to_root _ = ()
  let rank = 0
  let first = true
  let root_perform f = f ()

  let self_init_rng () =
    Random.self_init ();
    R.init_rng Random.(int 1000000000)


  let with_same_rng f = f ()
  let print = Printf.printf "%s%!"
  let print_endline = Printf.printf "%s\n%!"
end
