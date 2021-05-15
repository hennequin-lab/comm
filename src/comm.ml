module type T = sig
  (** number of nodes in the network *)
  val n_nodes : int

  (** wait until all nodes have called [barrier] *)
  val barrier : unit -> unit

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

(** {1 Single node} *)

(** Communication on a single node -- trivial, but can be used as a fallback *)
module Single (R : sig
  val init_rng : int -> unit
end) =
struct
  let n_nodes = 1
  let barrier () = ()
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

(** {1 Multi-node via the MPI library} *)

let trans x = Array.(init (length x.(0)) (fun j -> init (length x) (fun i -> x.(j).(i))))

let accumulate v =
  let n = Array.length v in
  assert (n > 0);
  let a = Array.make n 0. in
  a.(0) <- v.(0);
  for i = 1 to n - 1 do
    a.(i) <- a.(i - 1) +. v.(i)
  done;
  a


let swap v i j =
  let z = v.(j) in
  v.(j) <- v.(i);
  v.(i) <- z


let vshuffle v =
  let vc = Array.copy v in
  let n = Array.length vc in
  for i = n - 1 downto 1 do
    swap vc i (Random.int (i + 1))
  done;
  vc


(** Communication on multiple nodes via the mpi library *)
module Mpi (R : sig
  val init_rng : int -> unit
end) =
struct
  let n_nodes = Mpi.comm_size Mpi.comm_world
  let rank = Mpi.comm_rank Mpi.comm_world
  let first = rank = 0
  let root_perform f = if first then f () else ()
  let barrier () = Mpi.barrier Mpi.comm_world
  let gather x = Mpi.gather x 0 Mpi.comm_world

  let gatheroption x =
    let allx = gather x in
    if Array.length allx = 0
    then [||]
    else (
      let n = Array.length x in
      assert (Array.fold_left (fun accu z -> accu && Array.length z = n) true allx);
      let allx = trans allx in
      Array.init n (fun i ->
          let z =
            Array.fold_left
              (fun accu z ->
                match z with
                | Some x -> Some x
                | None -> accu)
              None
              allx.(i)
          in
          match z with
          | Some x -> x
          | None -> failwith "bad partitioning in gatheroption"))


  let allgather x = Mpi.allgather x Mpi.comm_world

  let allgatheroption x =
    let allx = allgather x in
    let n = Array.length x in
    assert (Array.fold_left (fun accu z -> accu && Array.length z = n) true allx);
    let allx = trans allx in
    Array.init n (fun i ->
        let z =
          Array.fold_left
            (fun accu z ->
              match z with
              | Some x -> Some x
              | None -> accu)
            None
            allx.(i)
        in
        match z with
        | Some x -> x
        | None -> failwith "bad partitioning in allgatheroption")


  let broadcast x = Mpi.broadcast x 0 Mpi.comm_world

  let broadcast' f =
    let z = if first then Some (f ()) else None in
    match broadcast z with
    | Some z -> z
    | None -> assert false


  let root_receive _ src = Mpi.receive src 0 Mpi.comm_world
  let send_to_root x = Mpi.send x 0 0 Mpi.comm_world

  let self_init_rng () =
    Random.self_init ();
    let n = Mpi.comm_size Mpi.comm_world in
    let maxseed = Nativeint.div Nativeint.max_int (Nativeint.of_int n) in
    let seeds =
      if first
      then (
        (* the trick is to sum-accumulate an array of 
            random strictly positive ints,
            so that each element is different from one another. This
            way, all nodes will have different seeds *)
        let z = Nativeint.sub maxseed 2n in
        let v =
          Array.init n (fun _ -> float (1 + Nativeint.to_int (Random.nativeint z)))
        in
        let v = accumulate v in
        Some (vshuffle v))
      else None
    in
    let seeds = Mpi.broadcast_opt seeds 0 Mpi.comm_world in
    let seed = int_of_float seeds.(Mpi.comm_rank Mpi.comm_world) in
    Random.init seed;
    R.init_rng seed


  let with_same_rng f =
    let seeds = allgather (Nativeint.to_int (Random.nativeint Nativeint.max_int)) in
    Random.init seeds.(0);
    R.init_rng seeds.(0);
    let res = f () in
    self_init_rng ();
    res


  let print s = if first then Printf.printf "%s%!" s else ()
  let print_endline s = if first then Printf.printf "%s\n%!" s else ()
end
