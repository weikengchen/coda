(* bin_io_unversioned *)

(* use [@@deriving bin_io_unversioned] for serialized types that
   are not send to nodes or persisted, namely the daemon RPC
   types for communication between the client and daemon, which
   are known to be running the same version of the software.

   The deriver here calls the deriver for bin_io after the lint
   phase. We avoid linter errors that would occur if
   we used [@@deriving bin_io] directly.
*)

open Core_kernel
open Ppxlib
open Versioned_util

let deriver = "bin_io_unversioned"

let get_bin_io_deriver () =
  match Ppx_deriving.lookup "bin_io" with
  | None ->
      failwith "bin_io_unversioned: could not find deriver for bin_io"
  | Some deriver ->
      deriver

let validate_type_decl inner2_modules type_decl =
  match inner2_modules with
  | [module_version; "Stable"] ->
      let inside_stable_versioned =
        try
          validate_module_version module_version type_decl.ptype_loc ;
          true
        with _ -> false
      in
      if inside_stable_versioned then
        Ppx_deriving.raise_errorf ~loc:type_decl.ptype_loc
          "Cannot use \"deriving bin_io_unversioned\" for a type defined in a \
           stable-versioned module"
  | _ ->
      ()

let rewrite_to_bin_io ~options ~path type_decls =
  ( if not (Int.equal (List.length type_decls) 1) then
    let type_decl1 = List.hd_exn type_decls in
    let type_decl2 = List.last_exn type_decls in
    let loc =
      { loc_start= type_decl1.ptype_loc.loc_start
      ; loc_end= type_decl2.ptype_loc.loc_end
      ; loc_ghost= false }
    in
    Ppx_deriving.raise_errorf ~loc
      "deriving bin_io_unversioned can only be used on a single type" ) ;
  let type_decl = List.hd_exn type_decls in
  ( if not @@ List.is_empty options then
    let loc = type_decl.ptype_loc in
    Ppx_deriving.raise_errorf ~loc
      "bin_io_unversioned does not take any options" ) ;
  let inner2_modules = List.take (List.rev path) 2 in
  validate_type_decl inner2_modules type_decl ;
  let bin_io_deriver = get_bin_io_deriver () in
  bin_io_deriver.type_decl_str ~options:[] ~path type_decls

let () =
  Ppx_deriving.(register (create deriver ~type_decl_str:rewrite_to_bin_io ()))

let _ =
  List.iter (Ppx_deriving.derivers ()) ~f:(fun d ->
      eprintf "DERIVER: %s\n%!" d.name )
