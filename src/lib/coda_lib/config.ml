open Async_kernel
open Coda_base
open Auxiliary_database
open Signature_lib

(* TODO: Pass banlist to modules discussed in Ban Reasons issue: https://github.com/CodaProtocol/coda/issues/852 *)

module Snark_worker_config = struct
  type t =
    { initial_snark_worker_key: Public_key.Compressed.t option
    ; shutdown_on_disconnect: bool
    ; num_threads: int option }
end

(** If ledger_db_location is None, will auto-generate a db based on a UUID *)
type t =
  { conf_dir: string
  ; logger: Logger.t
  ; pids: Child_processes.Termination.t
  ; trust_system: Trust_system.t
  ; monitor: Monitor.t option
  ; is_seed: bool
  ; initial_block_production_keypairs: Keypair.Set.t
  ; coinbase_receiver: [`Producer | `Other of Public_key.Compressed.t]
  ; work_selection_method: (module Work_selector.Selection_method_intf)
  ; snark_worker_config: Snark_worker_config.t
  ; work_reassignment_wait: int
  ; gossip_net_params: Gossip_net.Libp2p.Config.t
  ; net_config: Coda_networking.Config.t
  ; initial_fork_id: Fork_id.t
  ; snark_pool_disk_location: string
  ; wallets_disk_location: string
  ; persistent_root_location: string
  ; persistent_frontier_location: string
  ; staged_ledger_transition_backup_capacity: int [@default 10]
  ; time_controller: Block_time.Controller.t
  ; receipt_chain_database: Receipt_chain_database.t
  ; transaction_database: Transaction_database.t
  ; external_transition_database: External_transition_database.t
  ; snark_work_fee: Currency.Fee.t
  ; consensus_local_state: Consensus.Data.Local_state.t
  ; is_archive_rocksdb: bool [@default false]
  ; archive_process_location:
      Core.Host_and_port.t Cli_lib.Flag.Types.with_name option
        [@default None]
  ; demo_mode: bool [@default false]
  ; genesis_state_hash: State_hash.t
  ; log_block_creation: bool [@default false] }
[@@deriving make]
