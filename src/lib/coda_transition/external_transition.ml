open Async_kernel
open Core_kernel
open Coda_base
open Coda_state

module Validate_content = struct
  type t = bool -> unit

  let bin_read_t buf ~pos_ref = bin_read_unit buf ~pos_ref ; Fn.ignore

  let bin_write_t buf ~pos _ =
    let pos = bin_write_unit buf ~pos () in
    pos

  let bin_shape_t = bin_shape_unit

  let bin_size_t _ = bin_size_unit ()

  let t_of_sexp _ = Fn.ignore

  let sexp_of_t _ = sexp_of_unit ()

  let __versioned__ = ()
end

[%%versioned
module Stable = struct
  module V1 = struct
    type t =
      { protocol_state: Protocol_state.Value.Stable.V1.t
      ; protocol_state_proof: Proof.Stable.V1.t sexp_opaque
      ; staged_ledger_diff: Staged_ledger_diff.Stable.V1.t
      ; delta_transition_chain_proof:
          State_hash.Stable.V1.t * State_body_hash.Stable.V1.t list
      ; current_fork_id: Fork_id.Stable.V1.t
      ; next_fork_id_opt: Fork_id.Stable.V1.t option
      ; mutable validation_callback: Validate_content.t }
    [@@deriving sexp, fields]

    let to_latest = Fn.id

    let to_yojson
        { protocol_state
        ; protocol_state_proof= _
        ; staged_ledger_diff= _
        ; delta_transition_chain_proof= _
        ; current_fork_id
        ; next_fork_id_opt
        ; validation_callback= _ } =
      `Assoc
        [ ("protocol_state", Protocol_state.value_to_yojson protocol_state)
        ; ("protocol_state_proof", `String "<opaque>")
        ; ("staged_ledger_diff", `String "<opaque>")
        ; ("delta_transition_chain_proof", `String "<opaque>")
        ; ("current_fork_id", `String (Fork_id.to_string current_fork_id))
        ; ( "next_fork_id"
          , `String
              (Option.value_map next_fork_id_opt ~default:"<None>"
                 ~f:Fork_id.to_string) ) ]

    let delta_transition_chain_proof {delta_transition_chain_proof; _} =
      delta_transition_chain_proof

    let current_fork_id {current_fork_id; _} = current_fork_id

    let next_fork_id_opt {next_fork_id_opt; _} = next_fork_id_opt

    let consensus_state {protocol_state; _} =
      Protocol_state.consensus_state protocol_state

    let blockchain_state {protocol_state; _} =
      Protocol_state.blockchain_state protocol_state

    let blockchain_length t =
      consensus_state t |> Consensus.Data.Consensus_state.blockchain_length

    let state_hash {protocol_state; _} = Protocol_state.hash protocol_state

    let parent_hash {protocol_state; _} =
      Protocol_state.previous_state_hash protocol_state

    let consensus_time_produced_at t =
      consensus_state t |> Consensus.Data.Consensus_state.consensus_time

    let block_producer {staged_ledger_diff; _} =
      Staged_ledger_diff.creator staged_ledger_diff

    let transactions {staged_ledger_diff; _} =
      let open Staged_ledger.Pre_diff_info in
      match get_transactions staged_ledger_diff with
      | Ok transactions ->
          transactions
      | Error e ->
          Core.Error.raise (Error.to_error e)

    let user_commands {staged_ledger_diff; _} =
      Staged_ledger_diff.user_commands staged_ledger_diff

    let payments external_transition =
      List.filter
        (user_commands external_transition)
        ~f:(Fn.compose User_command_payload.is_payment User_command.payload)

    module T = struct
      type nonrec t = t

      let t_of_sexp = t_of_sexp

      let sexp_of_t = sexp_of_t

      let compare =
        Comparable.lift
          (fun existing candidate ->
            (* To prevent the logger to spam a lot of messsages, the logger input is set to null *)
            if Consensus.Data.Consensus_state.Value.equal existing candidate
            then 0
            else if
              `Keep
              = Consensus.Hooks.select ~existing ~candidate
                  ~logger:(Logger.null ())
            then -1
            else 1 )
          ~f:consensus_state
    end

    include Comparable.Make (T)
  end
end]

(* bin_io omitted *)
type t = Stable.Latest.t =
  { protocol_state: Protocol_state.Value.Stable.V1.t
  ; protocol_state_proof: Proof.Stable.V1.t sexp_opaque
  ; staged_ledger_diff: Staged_ledger_diff.t
  ; delta_transition_chain_proof: State_hash.t * State_body_hash.t list
  ; current_fork_id: Fork_id.t
  ; next_fork_id_opt: Fork_id.t option
  ; mutable validation_callback: Validate_content.t }
[@@deriving sexp]

(* another name, so we can avoid cyclic type below *)
type t_ = t

type external_transition = t

let broadcast {validation_callback; _} = validation_callback true

let don't_broadcast {validation_callback; _} = validation_callback false

let poke_validation_callback t cb = t.validation_callback <- cb

[%%define_locally
Stable.Latest.
  ( protocol_state
  , protocol_state_proof
  , delta_transition_chain_proof
  , current_fork_id
  , next_fork_id_opt
  , blockchain_state
  , blockchain_length
  , staged_ledger_diff
  , consensus_state
  , state_hash
  , parent_hash
  , consensus_time_produced_at
  , block_producer
  , transactions
  , user_commands
  , payments
  , to_yojson )]

include Comparable.Make (Stable.Latest)

let create ~protocol_state ~protocol_state_proof ~staged_ledger_diff
    ~delta_transition_chain_proof ~validation_callback ?next_fork_id_opt () =
  let current_fork_id =
    try Fork_id.get_current ()
    with _ ->
      failwith
        "Cannot create external transition before setting current fork id"
  in
  { protocol_state
  ; protocol_state_proof
  ; staged_ledger_diff
  ; delta_transition_chain_proof
  ; current_fork_id
  ; next_fork_id_opt
  ; validation_callback }

let timestamp {protocol_state; _} =
  Protocol_state.blockchain_state protocol_state |> Blockchain_state.timestamp

type fork_id_status =
  {valid_current: bool; valid_next: bool; matches_daemon: bool}

let fork_id_status {current_fork_id; next_fork_id_opt; _} =
  let valid_current = Fork_id.is_valid current_fork_id in
  let valid_next = Option.for_all next_fork_id_opt ~f:Fork_id.is_valid in
  let matches_daemon =
    Fork_id.equal current_fork_id (Fork_id.get_current ())
  in
  {valid_current; valid_next; matches_daemon}

module Validation = struct
  type ( 'time_received
       , 'genesis_state
       , 'proof
       , 'delta_transition_chain
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , 'fork_ids )
       t =
    'time_received
    * 'genesis_state
    * 'proof
    * 'delta_transition_chain
    * 'frontier_dependencies
    * 'staged_ledger_diff
    * 'fork_ids
    constraint 'time_received = [`Time_received] * (unit, _) Truth.t
    constraint 'genesis_state = [`Genesis_state] * (unit, _) Truth.t
    constraint 'proof = [`Proof] * (unit, _) Truth.t
    constraint
      'delta_transition_chain =
      [`Delta_transition_chain]
      * (State_hash.Stable.V1.t Non_empty_list.Stable.V1.t, _) Truth.t
    constraint
      'frontier_dependencies =
      [`Frontier_dependencies] * (unit, _) Truth.t
    constraint 'staged_ledger_diff = [`Staged_ledger_diff] * (unit, _) Truth.t
    constraint 'fork_ids = [`Fork_ids] * (unit, _) Truth.t

  type fully_invalid =
    ( [`Time_received] * unit Truth.false_t
    , [`Genesis_state] * unit Truth.false_t
    , [`Proof] * unit Truth.false_t
    , [`Delta_transition_chain] * State_hash.t Non_empty_list.t Truth.false_t
    , [`Frontier_dependencies] * unit Truth.false_t
    , [`Staged_ledger_diff] * unit Truth.false_t
    , [`Fork_ids] * unit Truth.false_t )
    t

  type fully_valid =
    ( [`Time_received] * unit Truth.true_t
    , [`Genesis_state] * unit Truth.true_t
    , [`Proof] * unit Truth.true_t
    , [`Delta_transition_chain] * State_hash.t Non_empty_list.t Truth.true_t
    , [`Frontier_dependencies] * unit Truth.true_t
    , [`Staged_ledger_diff] * unit Truth.true_t
    , [`Fork_ids] * unit Truth.true_t )
    t

  type ( 'time_received
       , 'genesis_state
       , 'proof
       , 'delta_transition_chain
       , 'frontier_dependencies
       , 'staged_ledger_diff
       , 'fork_ids )
       with_transition =
    (external_transition, State_hash.t) With_hash.t
    * ( 'time_received
      , 'genesis_state
      , 'proof
      , 'delta_transition_chain
      , 'frontier_dependencies
      , 'staged_ledger_diff
      , 'fork_ids )
      t

  let fully_invalid =
    ( (`Time_received, Truth.False)
    , (`Genesis_state, Truth.False)
    , (`Proof, Truth.False)
    , (`Delta_transition_chain, Truth.False)
    , (`Frontier_dependencies, Truth.False)
    , (`Staged_ledger_diff, Truth.False)
    , (`Fork_ids, Truth.False) )

  type initial_valid =
    ( [`Time_received] * unit Truth.true_t
    , [`Genesis_state] * unit Truth.true_t
    , [`Proof] * unit Truth.true_t
    , [`Delta_transition_chain] * State_hash.t Non_empty_list.t Truth.true_t
    , [`Frontier_dependencies] * unit Truth.false_t
    , [`Staged_ledger_diff] * unit Truth.false_t
    , [`Fork_ids] * unit Truth.true_t )
    t

  type almost_valid =
    ( [`Time_received] * unit Truth.true_t
    , [`Genesis_state] * unit Truth.true_t
    , [`Proof] * unit Truth.true_t
    , [`Delta_transition_chain] * State_hash.t Non_empty_list.t Truth.true_t
    , [`Frontier_dependencies] * unit Truth.true_t
    , [`Staged_ledger_diff] * unit Truth.false_t
    , [`Fork_ids] * unit Truth.true_t )
    t

  let wrap t = (t, fully_invalid)

  let extract_delta_transition_chain_witness = function
    | ( _
      , _
      , _
      , (`Delta_transition_chain, Truth.True delta_transition_chain_witness)
      , _
      , _
      , _ ) ->
        delta_transition_chain_witness
    | _ ->
        failwith "why can't this be refuted?"

  let reset_frontier_dependencies_validation (transition_with_hash, validation)
      =
    match validation with
    | ( time_received
      , genesis_state
      , proof
      , delta_transition_chain
      , (`Frontier_dependencies, Truth.True ())
      , staged_ledger_diff
      , fork_ids ) ->
        ( transition_with_hash
        , ( time_received
          , genesis_state
          , proof
          , delta_transition_chain
          , (`Frontier_dependencies, Truth.False)
          , staged_ledger_diff
          , fork_ids ) )
    | _ ->
        failwith "why can't this be refuted?"

  let reset_staged_ledger_diff_validation (transition_with_hash, validation) =
    match validation with
    | ( time_received
      , genesis_state
      , proof
      , delta_transition_chain
      , frontier_dependencies
      , (`Staged_ledger_diff, Truth.True ())
      , fork_ids ) ->
        ( transition_with_hash
        , ( time_received
          , genesis_state
          , proof
          , delta_transition_chain
          , frontier_dependencies
          , (`Staged_ledger_diff, Truth.False)
          , fork_ids ) )
    | _ ->
        failwith "why can't this be refuted?"

  let forget_validation (t, _) = With_hash.data t

  module Unsafe = struct
    let set_valid_time_received :
           ( [`Time_received] * unit Truth.false_t
           , 'genesis_state
           , 'proof
           , 'delta_transition_chain
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , 'fork_ids )
           t
        -> ( [`Time_received] * unit Truth.true_t
           , 'genesis_state
           , 'proof
           , 'delta_transition_chain
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , 'fork_ids )
           t = function
      | ( (`Time_received, Truth.False)
        , genesis_state
        , proof
        , delta_transition_chain
        , frontier_dependencies
        , staged_ledger_diff
        , fork_ids ) ->
          ( (`Time_received, Truth.True ())
          , genesis_state
          , proof
          , delta_transition_chain
          , frontier_dependencies
          , staged_ledger_diff
          , fork_ids )
      | _ ->
          failwith "why can't this be refuted?"

    let set_valid_proof :
           ( 'time_received
           , 'genesis_state
           , [`Proof] * unit Truth.false_t
           , 'delta_transition_chain
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , 'fork_ids )
           t
        -> ( 'time_received
           , 'genesis_state
           , [`Proof] * unit Truth.true_t
           , 'delta_transition_chain
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , 'fork_ids )
           t = function
      | ( time_received
        , genesis_state
        , (`Proof, Truth.False)
        , delta_transition_chain
        , frontier_dependencies
        , staged_ledger_diff
        , fork_ids ) ->
          ( time_received
          , genesis_state
          , (`Proof, Truth.True ())
          , delta_transition_chain
          , frontier_dependencies
          , staged_ledger_diff
          , fork_ids )
      | _ ->
          failwith "why can't this be refuted?"

    let set_valid_genesis_state :
           ( 'time_received
           , [`Genesis_state] * unit Truth.false_t
           , 'proof
           , 'delta_transition_chain
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , 'fork_ids )
           t
        -> ( 'time_received
           , [`Genesis_state] * unit Truth.true_t
           , 'proof
           , 'delta_transition_chain
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , 'fork_ids )
           t = function
      | ( time_received
        , (`Genesis_state, Truth.False)
        , proof
        , delta_transition_chain
        , frontier_dependencies
        , staged_ledger_diff
        , fork_ids ) ->
          ( time_received
          , (`Genesis_state, Truth.True ())
          , proof
          , delta_transition_chain
          , frontier_dependencies
          , staged_ledger_diff
          , fork_ids )
      | _ ->
          failwith "why can't this be refuted?"

    let set_valid_delta_transition_chain :
           ( 'time_received
           , 'genesis_state
           , 'proof
           , [`Delta_transition_chain]
             * State_hash.t Non_empty_list.t Truth.false_t
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , 'fork_ids )
           t
        -> State_hash.t Non_empty_list.t
        -> ( 'time_received
           , 'genesis_state
           , 'proof
           , [`Delta_transition_chain]
             * State_hash.t Non_empty_list.t Truth.true_t
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , 'fork_ids )
           t =
     fun validation hashes ->
      match validation with
      | ( time_received
        , genesis_state
        , proof
        , (`Delta_transition_chain, Truth.False)
        , frontier_dependencies
        , staged_ledger_diff
        , fork_ids ) ->
          ( time_received
          , genesis_state
          , proof
          , (`Delta_transition_chain, Truth.True hashes)
          , frontier_dependencies
          , staged_ledger_diff
          , fork_ids )
      | _ ->
          failwith "why can't this be refuted?"

    let set_valid_frontier_dependencies :
           ( 'time_received
           , 'genesis_state
           , 'proof
           , 'delta_transition_chain
           , [`Frontier_dependencies] * unit Truth.false_t
           , 'staged_ledger_diff
           , 'fork_ids )
           t
        -> ( 'time_received
           , 'genesis_state
           , 'proof
           , 'delta_transition_chain
           , [`Frontier_dependencies] * unit Truth.true_t
           , 'staged_ledger_diff
           , 'fork_ids )
           t = function
      | ( time_received
        , genesis_state
        , proof
        , delta_transition_chain
        , (`Frontier_dependencies, Truth.False)
        , staged_ledger_diff
        , fork_ids ) ->
          ( time_received
          , genesis_state
          , proof
          , delta_transition_chain
          , (`Frontier_dependencies, Truth.True ())
          , staged_ledger_diff
          , fork_ids )
      | _ ->
          failwith "why can't this be refuted?"

    let set_valid_staged_ledger_diff :
           ( 'time_received
           , 'genesis_state
           , 'proof
           , 'delta_transition_chain
           , 'frontier_dependencies
           , [`Staged_ledger_diff] * unit Truth.false_t
           , 'fork_ids )
           t
        -> ( 'time_received
           , 'genesis_state
           , 'proof
           , 'delta_transition_chain
           , 'frontier_dependencies
           , [`Staged_ledger_diff] * unit Truth.true_t
           , 'fork_ids )
           t = function
      | ( time_received
        , genesis_state
        , proof
        , delta_transition_chain
        , frontier_dependencies
        , (`Staged_ledger_diff, Truth.False)
        , fork_ids ) ->
          ( time_received
          , genesis_state
          , proof
          , delta_transition_chain
          , frontier_dependencies
          , (`Staged_ledger_diff, Truth.True ())
          , fork_ids )
      | _ ->
          failwith "why can't this be refuted?"

    let set_valid_fork_ids :
           ( 'time_received
           , 'genesis_state
           , 'proof
           , 'delta_transition_chain
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , [`Fork_ids] * unit Truth.false_t )
           t
        -> ( 'time_received
           , 'genesis_state
           , 'proof
           , 'delta_transition_chain
           , 'frontier_dependencies
           , 'staged_ledger_diff
           , [`Fork_ids] * unit Truth.true_t )
           t = function
      | ( time_received
        , genesis_state
        , proof
        , delta_transition_chain
        , frontier_dependencies
        , staged_ledger_diff
        , (`Fork_ids, Truth.False) ) ->
          ( time_received
          , genesis_state
          , proof
          , delta_transition_chain
          , frontier_dependencies
          , staged_ledger_diff
          , (`Fork_ids, Truth.True ()) )
      | _ ->
          failwith "why can't this be refuted?"
  end
end

let skip_time_received_validation `This_transition_was_not_received_via_gossip
    (t, validation) =
  (t, Validation.Unsafe.set_valid_time_received validation)

let skip_genesis_protocol_state_validation
    `This_transition_was_generated_internally (t, validation) =
  (t, Validation.Unsafe.set_valid_genesis_state validation)

let validate_time_received (t, validation) ~time_received =
  let consensus_state =
    With_hash.data t |> protocol_state |> Protocol_state.consensus_state
  in
  let received_unix_timestamp =
    Block_time.to_span_since_epoch time_received |> Block_time.Span.to_ms
  in
  match
    Consensus.Hooks.received_at_valid_time consensus_state
      ~time_received:received_unix_timestamp
  with
  | Ok () ->
      Ok (t, Validation.Unsafe.set_valid_time_received validation)
  | Error err ->
      Error (`Invalid_time_received err)

let skip_proof_validation `This_transition_was_generated_internally
    (t, validation) =
  (t, Validation.Unsafe.set_valid_proof validation)

let skip_delta_transition_chain_validation
    `This_transition_was_not_received_via_gossip (t, validation) =
  let previous_protocol_state_hash = With_hash.data t |> parent_hash in
  ( t
  , Validation.Unsafe.set_valid_delta_transition_chain validation
      (Non_empty_list.singleton previous_protocol_state_hash) )

let validate_genesis_protocol_state ~genesis_state_hash (t, validation) =
  let {protocol_state= state; _} = With_hash.data t in
  if
    State_hash.equal
      (Protocol_state.genesis_state_hash state)
      genesis_state_hash
  then Ok (t, Validation.Unsafe.set_valid_genesis_state validation)
  else Error `Invalid_genesis_protocol_state

let validate_proof (t, validation) ~verifier =
  let open Blockchain_snark.Blockchain in
  let open Deferred.Let_syntax in
  let {protocol_state= state; protocol_state_proof= proof; _} =
    With_hash.data t
  in
  match%map Verifier.verify_blockchain_snark verifier {state; proof} with
  | Ok verified ->
      if verified then Ok (t, Validation.Unsafe.set_valid_proof validation)
      else Error `Invalid_proof
  | Error e ->
      Error (`Verifier_error e)

let validate_delta_transition_chain (t, validation) =
  let transition = With_hash.data t in
  match
    Transition_chain_verifier.verify ~target_hash:(parent_hash transition)
      ~transition_chain_proof:transition.delta_transition_chain_proof
  with
  | Some hashes ->
      Ok
        ( t
        , Validation.Unsafe.set_valid_delta_transition_chain validation hashes
        )
  | None ->
      Error `Invalid_delta_transition_chain_proof

let validate_fork_ids (t, validation) =
  let {valid_current; valid_next; matches_daemon} =
    fork_id_status (With_hash.data t)
  in
  if not (valid_current && valid_next) then Error `Invalid_fork_id
  else if not matches_daemon then Error `Mismatched_fork_id
  else Ok (t, Validation.Unsafe.set_valid_fork_ids validation)

let skip_frontier_dependencies_validation
    (_ :
      [ `This_transition_belongs_to_a_detached_subtree
      | `This_transition_was_loaded_from_persistence ]) (t, validation) =
  (t, Validation.Unsafe.set_valid_frontier_dependencies validation)

let validate_staged_ledger_hash
    (`Staged_ledger_already_materialized staged_ledger_hash) (t, validation) =
  if
    Staged_ledger_hash.equal staged_ledger_hash
      (Blockchain_state.staged_ledger_hash
         (blockchain_state (With_hash.data t)))
  then Ok (t, Validation.Unsafe.set_valid_staged_ledger_diff validation)
  else Error `Staged_ledger_hash_mismatch

let skip_staged_ledger_diff_validation
    `This_transition_has_a_trusted_staged_ledger (t, validation) =
  (t, Validation.Unsafe.set_valid_staged_ledger_diff validation)

let skip_fork_ids_validation `This_transition_has_valid_fork_ids (t, validation)
    =
  (t, Validation.Unsafe.set_valid_fork_ids validation)

module With_validation = struct
  let compare (t1, _) (t2, _) = compare (With_hash.data t1) (With_hash.data t2)

  let state_hash (t, _) = With_hash.hash t

  let lift f (t, _) = With_hash.data t |> f

  let protocol_state t = lift protocol_state t

  let protocol_state_proof t = lift protocol_state_proof t

  let blockchain_state t = lift blockchain_state t

  let blockchain_length t = lift blockchain_length t

  let staged_ledger_diff t = lift staged_ledger_diff t

  let consensus_state t = lift consensus_state t

  let parent_hash t = lift parent_hash t

  let consensus_time_produced_at t = lift consensus_time_produced_at t

  let block_producer t = lift block_producer t

  let user_commands t = lift user_commands t

  let transactions t = lift transactions t

  let payments t = lift payments t

  let delta_transition_chain_proof t = lift delta_transition_chain_proof t

  let current_fork_id t = lift current_fork_id t

  let next_fork_id_opt t = lift next_fork_id_opt t

  let fork_id_status t = lift fork_id_status t

  let broadcast t = lift broadcast t

  let don't_broadcast t = lift don't_broadcast t
end

module Initial_validated = struct
  type t =
    (external_transition, State_hash.t) With_hash.t * Validation.initial_valid

  type nonrec fork_id_status = fork_id_status =
    {valid_current: bool; valid_next: bool; matches_daemon: bool}

  include With_validation
end

module Almost_validated = struct
  type t =
    (external_transition, State_hash.t) With_hash.t * Validation.almost_valid

  type nonrec fork_id_status = fork_id_status =
    {valid_current: bool; valid_next: bool; matches_daemon: bool}

  include With_validation
end

module Validated = struct
  [%%versioned_binable
  module Stable = struct
    module V1 = struct
      type t =
        (t_, State_hash.t) With_hash.t
        * ( [`Time_received] * (unit, Truth.True.t) Truth.t
          , [`Genesis_state] * (unit, Truth.True.t) Truth.t
          , [`Proof] * (unit, Truth.True.t) Truth.t
          , [`Delta_transition_chain]
            * (State_hash.t Non_empty_list.t, Truth.True.t) Truth.t
          , [`Frontier_dependencies] * (unit, Truth.True.t) Truth.t
          , [`Staged_ledger_diff] * (unit, Truth.True.t) Truth.t
          , [`Fork_ids] * (unit, Truth.True.t) Truth.t )
          Validation.t

      let to_latest = Fn.id

      module Erased = struct
        (* if this type receives a new version, that changes the serialization of
             the type `t', so that type must also get a new version
        *)
        [%%versioned
        module Stable = struct
          module V1 = struct
            type t =
              (Stable.V1.t, State_hash.Stable.V1.t) With_hash.Stable.V1.t
              * State_hash.Stable.V1.t Non_empty_list.Stable.V1.t
            [@@deriving sexp]

            let to_latest = Fn.id
          end
        end]
      end

      type erased = Erased.Stable.Latest.t [@@deriving sexp]

      let erase (transition_with_hash, validation) =
        ( transition_with_hash
        , Validation.extract_delta_transition_chain_witness validation )

      let elaborate (transition_with_hash, delta_transition_chain_witness) =
        ( transition_with_hash
        , ( (`Time_received, Truth.True ())
          , (`Genesis_state, Truth.True ())
          , (`Proof, Truth.True ())
          , (`Delta_transition_chain, Truth.True delta_transition_chain_witness)
          , (`Frontier_dependencies, Truth.True ())
          , (`Staged_ledger_diff, Truth.True ())
          , (`Fork_ids, Truth.True ()) ) )

      include Sexpable.Of_sexpable (struct
                  type t = erased [@@deriving sexp]
                end)
                (struct
                  type nonrec t = t

                  let of_sexpable = elaborate

                  let to_sexpable = erase
                end)

      include Binable.Of_binable (struct
                  type t = Erased.Stable.Latest.t [@@deriving bin_io]
                end)
                (struct
                  type nonrec t = t

                  let of_binable = elaborate

                  let to_binable = erase
                end)

      let to_yojson (transition_with_hash, _) =
        With_hash.to_yojson to_yojson State_hash.to_yojson transition_with_hash

      let create_unsafe_pre_hashed t =
        `I_swear_this_is_safe_see_my_comment
          ( Validation.wrap t
          |> skip_time_received_validation
               `This_transition_was_not_received_via_gossip
          |> skip_genesis_protocol_state_validation
               `This_transition_was_generated_internally
          |> skip_proof_validation `This_transition_was_generated_internally
          |> skip_delta_transition_chain_validation
               `This_transition_was_not_received_via_gossip
          |> skip_frontier_dependencies_validation
               `This_transition_belongs_to_a_detached_subtree
          |> skip_staged_ledger_diff_validation
               `This_transition_has_a_trusted_staged_ledger
          |> skip_fork_ids_validation `This_transition_has_valid_fork_ids )

      let create_unsafe t =
        create_unsafe_pre_hashed (With_hash.of_data t ~hash_data:state_hash)

      include With_validation
    end
  end]

  type t = Stable.Latest.t

  type nonrec fork_id_status = fork_id_status =
    {valid_current: bool; valid_next: bool; matches_daemon: bool}

  [%%define_locally
  Stable.Latest.
    ( sexp_of_t
    , t_of_sexp
    , create_unsafe_pre_hashed
    , create_unsafe
    , protocol_state
    , delta_transition_chain_proof
    , current_fork_id
    , next_fork_id_opt
    , fork_id_status
    , broadcast
    , don't_broadcast
    , protocol_state_proof
    , blockchain_state
    , blockchain_length
    , staged_ledger_diff
    , consensus_state
    , state_hash
    , parent_hash
    , consensus_time_produced_at
    , block_producer
    , transactions
    , user_commands
    , payments
    , erase
    , to_yojson )]

  include Comparable.Make (Stable.Latest)

  let to_initial_validated t =
    t |> Validation.reset_frontier_dependencies_validation
    |> Validation.reset_staged_ledger_diff_validation
end

let genesis ~genesis_ledger ~base_proof =
  let genesis_protocol_state =
    Coda_state.Genesis_protocol_state.t ~genesis_ledger
  in
  let creator = fst Consensus_state_hooks.genesis_winner in
  let empty_diff =
    { Staged_ledger_diff.diff=
        ( { completed_works= []
          ; user_commands= []
          ; coinbase= Staged_ledger_diff.At_most_two.Zero }
        , None )
    ; creator
    ; coinbase_receiver= creator }
  in
  (* the genesis transition is assumed to be valid *)
  let (`I_swear_this_is_safe_see_my_comment transition) =
    Validated.create_unsafe_pre_hashed
      (With_hash.map genesis_protocol_state ~f:(fun protocol_state ->
           create ~protocol_state ~protocol_state_proof:base_proof
             ~staged_ledger_diff:empty_diff ~validation_callback:Fn.ignore
             ~delta_transition_chain_proof:
               (Protocol_state.previous_state_hash protocol_state, [])
             () ))
  in
  transition

module For_tests = struct
  let create ~protocol_state ~protocol_state_proof ~staged_ledger_diff
      ~delta_transition_chain_proof ~validation_callback ?next_fork_id_opt () =
    Fork_id.(set_current empty) ;
    create ~protocol_state ~protocol_state_proof ~staged_ledger_diff
      ~delta_transition_chain_proof ~validation_callback ?next_fork_id_opt ()

  let genesis ~genesis_ledger ~base_proof =
    Fork_id.(set_current empty) ;
    genesis ~genesis_ledger ~base_proof
end

module Transition_frontier_validation (Transition_frontier : sig
  type t

  module Breadcrumb : sig
    type t

    val validated_transition : t -> Validated.t
  end

  val root : t -> Breadcrumb.t

  val find : t -> State_hash.t -> Breadcrumb.t option
end) =
struct
  let validate_frontier_dependencies (t, validation) ~logger ~frontier =
    let open Result.Let_syntax in
    let hash = With_hash.hash t in
    let protocol_state = protocol_state (With_hash.data t) in
    let parent_hash = Protocol_state.previous_state_hash protocol_state in
    let root_protocol_state =
      Transition_frontier.root frontier
      |> Transition_frontier.Breadcrumb.validated_transition
      |> Validated.protocol_state
    in
    let%bind () =
      Result.ok_if_true
        (Transition_frontier.find frontier hash |> Option.is_none)
        ~error:`Already_in_frontier
    in
    let%bind () =
      (* need pervasive (=) in scope for comparing polymorphic variant *)
      let ( = ) = Pervasives.( = ) in
      Result.ok_if_true
        ( `Take
        = Consensus.Hooks.select
            ~logger:
              (Logger.extend logger
                 [ ( "selection_context"
                   , `String
                       "External_transition.Transition_frontier_validation.validate_frontier_dependencies"
                   ) ])
            ~existing:(Protocol_state.consensus_state root_protocol_state)
            ~candidate:(Protocol_state.consensus_state protocol_state) )
        ~error:`Not_selected_over_frontier_root
    in
    let%map () =
      Result.ok_if_true
        (Transition_frontier.find frontier parent_hash |> Option.is_some)
        ~error:`Parent_missing_from_frontier
    in
    (t, Validation.Unsafe.set_valid_frontier_dependencies validation)
end

module Staged_ledger_validation = struct
  let target_hash_of_ledger_proof =
    let open Ledger_proof in
    Fn.compose statement_target statement

  let validate_staged_ledger_diff :
         ( 'time_received
         , 'genesis_state
         , 'proof
         , 'delta_transition_chain
         , 'frontier_dependencies
         , [`Staged_ledger_diff] * unit Truth.false_t
         , 'fork_ids )
         Validation.with_transition
      -> logger:Logger.t
      -> verifier:Verifier.t
      -> parent_staged_ledger:Staged_ledger.t
      -> parent_protocol_state:Protocol_state.value
      -> ( [`Just_emitted_a_proof of bool]
           * [ `External_transition_with_validation of
               ( 'time_received
               , 'genesis_state
               , 'proof
               , 'delta_transition_chain
               , 'frontier_dependencies
               , [`Staged_ledger_diff] * unit Truth.true_t
               , 'fork_ids )
               Validation.with_transition ]
           * [`Staged_ledger of Staged_ledger.t]
         , [ `Invalid_staged_ledger_diff of
             [ `Incorrect_target_staged_ledger_hash
             | `Incorrect_target_snarked_ledger_hash ]
             list
           | `Staged_ledger_application_failed of
             Staged_ledger.Staged_ledger_error.t ] )
         Deferred.Result.t =
   fun (t, validation) ~logger ~verifier ~parent_staged_ledger
       ~parent_protocol_state ->
    let open Deferred.Result.Let_syntax in
    let transition = With_hash.data t in
    let blockchain_state =
      Protocol_state.blockchain_state (protocol_state transition)
    in
    let staged_ledger_diff = staged_ledger_diff transition in
    let%bind ( `Hash_after_applying staged_ledger_hash
             , `Ledger_proof proof_opt
             , `Staged_ledger transitioned_staged_ledger
             , `Pending_coinbase_data _ ) =
      Staged_ledger.apply ~logger ~verifier parent_staged_ledger
        staged_ledger_diff
        ~state_body_hash:
          Protocol_state.(Body.hash @@ body parent_protocol_state)
      |> Deferred.Result.map_error ~f:(fun e ->
             `Staged_ledger_application_failed e )
    in
    let target_ledger_hash =
      match proof_opt with
      | None ->
          Option.value_map
            (Staged_ledger.current_ledger_proof transitioned_staged_ledger)
            ~f:target_hash_of_ledger_proof
            ~default:
              (Frozen_ledger_hash.of_ledger_hash
                 (Ledger.merkle_root (Lazy.force Test_genesis_ledger.t)))
      | Some (proof, _) ->
          target_hash_of_ledger_proof proof
    in
    let maybe_errors =
      Option.all
        [ Option.some_if
            (not
               (Staged_ledger_hash.equal staged_ledger_hash
                  (Blockchain_state.staged_ledger_hash blockchain_state)))
            `Incorrect_target_staged_ledger_hash
        ; Option.some_if
            (not
               (Frozen_ledger_hash.equal target_ledger_hash
                  (Blockchain_state.snarked_ledger_hash blockchain_state)))
            `Incorrect_target_snarked_ledger_hash ]
    in
    Deferred.return
      ( match maybe_errors with
      | Some errors ->
          Error (`Invalid_staged_ledger_diff errors)
      | None ->
          Ok
            ( `Just_emitted_a_proof (Option.is_some proof_opt)
            , `External_transition_with_validation
                (t, Validation.Unsafe.set_valid_staged_ledger_diff validation)
            , `Staged_ledger transitioned_staged_ledger ) )
end
