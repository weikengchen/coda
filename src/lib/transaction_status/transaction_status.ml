open Core_kernel
open Coda_base
open Pipe_lib
open Network_pool

module State = struct
  [%%versioned
  module Stable = struct
    module V1 = struct
      type t = Pending | Included | Unknown [@@deriving equal, sexp, compare]

      let to_latest = Fn.id
    end
  end]

  type t = Stable.Latest.t = Pending | Included | Unknown
  [@@deriving equal, sexp, compare]

  let to_string = function
    | Pending ->
        "PENDING"
    | Included ->
        "INCLUDED"
    | Unknown ->
        "UNKOWN"
end

(* TODO: this is extremely expensive as implemented and needs to be replaced with an extension *)
let get_status ~frontier_broadcast_pipe ~transaction_pool cmd =
  let open Or_error.Let_syntax in
  let%map check_cmd =
    Result.of_option (User_command.check cmd)
      ~error:(Error.of_string "Invalid signature")
  in
  let resource_pool = Transaction_pool.resource_pool transaction_pool in
  match Broadcast_pipe.Reader.peek frontier_broadcast_pipe with
  | None ->
      State.Unknown
  | Some transition_frontier ->
      with_return (fun {return} ->
          let best_tip_path =
            Transition_frontier.best_tip_path transition_frontier
          in
          let best_tip_user_commands =
            Sequence.fold (Sequence.of_list best_tip_path)
              ~init:User_command.Set.empty ~f:(fun acc_set breadcrumb ->
                let user_commands =
                  Transition_frontier.Breadcrumb.user_commands breadcrumb
                in
                List.fold user_commands ~init:acc_set ~f:Set.add )
          in
          if Set.mem best_tip_user_commands cmd then return State.Included ;
          let all_transactions =
            Transition_frontier.(
              Breadcrumb.all_user_commands
                (Transition_frontier.all_breadcrumbs transition_frontier))
          in
          if Set.mem all_transactions cmd then return State.Pending ;
          if Transaction_pool.Resource_pool.member resource_pool check_cmd then
            return State.Pending ;
          State.Unknown )

let%test_module "transaction_status" =
  ( module struct
    open Async
    open Coda_numbers

    let max_length = 10

    let frontier_size = 1

    let logger = Logger.null ()

    let trust_system = Trust_system.null ()

    let key_gen =
      let open Quickcheck.Generator in
      let open Quickcheck.Generator.Let_syntax in
      let keypairs =
        List.map (Lazy.force Test_genesis_ledger.accounts) ~f:fst
      in
      let%map random_key_opt = of_list keypairs in
      ( Test_genesis_ledger.largest_account_keypair_exn ()
      , Signature_lib.Keypair.of_private_key_exn
          (Option.value_exn random_key_opt) )

    let gen_frontier =
      Transition_frontier.For_tests.gen ~logger ~trust_system ~max_length
        ~size:frontier_size ()

    let gen_user_command =
      User_command.Gen.payment ~sign_type:`Real ~max_amount:100 ~max_fee:10
        ~key_gen ~nonce:(Account_nonce.of_int 1) ()

    let create_pool ~frontier_broadcast_pipe =
      let pool_reader, _ =
        Strict_pipe.(
          create ~name:"transaction_status incomming diff" Synchronous)
      in
      let local_reader, local_writer =
        Strict_pipe.(create ~name:"transaction_status local diff" Synchronous)
      in
      let config = Transaction_pool.Resource_pool.make_config ~trust_system in
      let transaction_pool =
        Transaction_pool.create ~config ~incoming_diffs:pool_reader ~logger
          ~local_diffs:local_reader ~frontier_broadcast_pipe
      in
      don't_wait_for
      @@ Linear_pipe.iter (Transaction_pool.broadcasts transaction_pool)
           ~f:(fun transactions ->
             Logger.trace logger
               "Transactions have been applied successfully and is propagated \
                throughout the 'network'"
               ~module_:__MODULE__ ~location:__LOC__
               ~metadata:
                 [ ( "transactions"
                   , Transaction_pool.Resource_pool.Diff.to_yojson transactions
                   ) ] ;
             Deferred.unit ) ;
      (* Need to wait for transaction_pool to see the transition_frontier *)
      let%map () = Async.Scheduler.yield_until_no_jobs_remain () in
      (transaction_pool, local_writer)

    let%test_unit "If the transition frontier currently doesn't exist, the \
                   status of a sent transaction will be unknown" =
      Quickcheck.test ~trials:1 gen_user_command ~f:(fun user_command ->
          Async.Thread_safe.block_on_async_exn (fun () ->
              let frontier_broadcast_pipe, _ = Broadcast_pipe.create None in
              let%bind transaction_pool, local_diffs_writer =
                create_pool ~frontier_broadcast_pipe
              in
              let%bind () =
                Strict_pipe.Writer.write local_diffs_writer
                  ([user_command], Fn.const ())
              in
              let%map () = Async.Scheduler.yield_until_no_jobs_remain () in
              Logger.info logger "Checking status" ~module_:__MODULE__
                ~location:__LOC__ ;
              [%test_eq: State.t] ~equal:State.equal State.Unknown
                ( Or_error.ok_exn
                @@ get_status ~frontier_broadcast_pipe ~transaction_pool
                     user_command ) ) )

    let%test_unit "A pending transaction is either in the transition frontier \
                   or transaction pool, but not in the best path of the \
                   transition frontier" =
      Quickcheck.test ~trials:1
        (Quickcheck.Generator.tuple2 gen_frontier gen_user_command)
        ~f:(fun (frontier, user_command) ->
          Async.Thread_safe.block_on_async_exn (fun () ->
              let frontier_broadcast_pipe, _ =
                Broadcast_pipe.create (Some frontier)
              in
              let%bind transaction_pool, local_diffs_writer =
                create_pool ~frontier_broadcast_pipe
              in
              let%bind () =
                Strict_pipe.Writer.write local_diffs_writer
                  ([user_command], Fn.const ())
              in
              let%map () = Async.Scheduler.yield_until_no_jobs_remain () in
              let status =
                Or_error.ok_exn
                @@ get_status ~frontier_broadcast_pipe ~transaction_pool
                     user_command
              in
              Logger.info logger "Computing status" ~module_:__MODULE__
                ~location:__LOC__ ;
              [%test_eq: State.t] ~equal:State.equal State.Pending status ) )

    let%test_unit "An unknown transaction does not appear in the transition \
                   frontier or transaction pool " =
      let user_commands_generator =
        let open Quickcheck.Generator in
        let open Let_syntax in
        let%bind head_user_command = gen_user_command in
        let%map tail_user_commands =
          Quickcheck.Generator.list_with_length 10 gen_user_command
        in
        Non_empty_list.init head_user_command tail_user_commands
      in
      Quickcheck.test ~trials:1
        (Quickcheck.Generator.tuple2 gen_frontier user_commands_generator)
        ~f:(fun (frontier, user_commands) ->
          Async.Thread_safe.block_on_async_exn (fun () ->
              let frontier_broadcast_pipe, _ =
                Broadcast_pipe.create (Some frontier)
              in
              let%bind transaction_pool, local_diffs_writer =
                create_pool ~frontier_broadcast_pipe
              in
              let unknown_user_command, pool_user_commands =
                Non_empty_list.uncons user_commands
              in
              let%bind () =
                Strict_pipe.Writer.write local_diffs_writer
                  (pool_user_commands, Fn.const ())
              in
              let%map () = Async.Scheduler.yield_until_no_jobs_remain () in
              Logger.info logger "Computing status" ~module_:__MODULE__
                ~location:__LOC__ ;
              [%test_eq: State.t] ~equal:State.equal State.Unknown
                ( Or_error.ok_exn
                @@ get_status ~frontier_broadcast_pipe ~transaction_pool
                     unknown_user_command ) ) )
  end )
