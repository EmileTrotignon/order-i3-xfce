(* Dispatch following the message type, and send the corresponding IPC
   message. Then simply output the reply.
*)
open Printf

let get_group_and_title (n : I3ipc.Reply.node) =
  match n.window_properties with
  | None -> ("", "")
  | Some prop ->
      (Option.value prop.class_ ~default:"", Option.value prop.title ~default:"")

let lnq_lex s1 s2 =
  if String.equal s1 s2 then false
  else
    let rec aux i1 i2 =
      if i1 = String.length s1 then true
      else if i2 = String.length s2 then false
      else
        let c1 = Char.code (Char.lowercase_ascii s1.[i1]) in
        let c2 = Char.code (Char.lowercase_ascii s2.[i2]) in
        if c1 = c2 then aux (i1 + 1) (i2 + 1) else c1 < c2
    in
    aux 0 0

let compare_lex s1 s2 =
  if String.equal s1 s2 then 0
  else
    let rec aux i1 i2 =
      if i1 = String.length s1 then 1
      else if i2 = String.length s2 then -1
      else
        let c1 = Char.code (Char.lowercase_ascii s1.[i1]) in
        let c2 = Char.code (Char.lowercase_ascii s2.[i2]) in
        if c1 = c2 then aux (i1 + 1) (i2 + 1) else Int.compare c1 c2
    in
    aux 0 0

let get_focused_window (tree : I3ipc.Reply.node) =
  let rec aux (node : I3ipc.Reply.node) =
    if node.focused then Some node
    else
      let nodeoptopt =
        List.find_opt
          (fun (n : I3ipc.Reply.node option) ->
            match n with Some (_ : I3ipc.Reply.node) -> true | None -> false)
          (List.map aux node.nodes)
      in
      match nodeoptopt with
      | None -> (
          let nodeoptopt =
            List.find_opt
              (fun (n : I3ipc.Reply.node option) ->
                match n with
                | Some (_ : I3ipc.Reply.node) -> true
                | None -> false)
              (List.map aux node.floating_nodes)
          in
          match nodeoptopt with
          | None -> None
          | Some None -> failwith "oooooops"
          | Some s -> s )
      | Some None -> failwith "ooops"
      | Some s -> s
  in
  match aux tree with Some node -> node | None -> failwith "no focused node"

let focus_win conn con_id =
  I3ipc.command conn (sprintf {|[con_id="%s"] focus|} con_id)

let move_window_left conn con_id =
  (let%lwt _ = focus_win conn con_id in
   Lwt.return_unit);%lwt
  let%lwt _ = I3ipc.command conn "move left" in
  Lwt.return_unit

let order_tab conn _ =
  let%lwt tree = I3ipc.get_tree conn in
  let rec iterator (node : I3ipc.Reply.node) =
    if node.layout = I3ipc.Reply.Tabbed then
      let tab = Array.of_list node.nodes in
      let compare n1 n2 =
        let group1, name1 = get_group_and_title n1 in
        let group2, name2 = get_group_and_title n2 in
        if compare_lex group1 group2 = 0 then compare_lex name1 name2
        else compare_lex group1 group2
      in
      Order.ordered tab compare (fun (n : I3ipc.Reply.node) ->
          let g, t = get_group_and_title n in
          printf "moving %s %s\n" g t;
          move_window_left conn n.id)
    else Lwt_list.iter_s iterator node.nodes
  in
  Lwt_list.iter_s iterator tree.nodes

let main =
  let format = Format.err_formatter in
  let%lwt conn = I3ipc.connect () in
  let%lwt _ = I3ipc.subscribe conn [ I3ipc.Window ] in
  while%lwt true do
    match%lwt I3ipc.next_event conn with
    | Window info -> (
        match info.change with
        | New | Title | FullscreenMode | Move ->
            let%lwt tree = I3ipc.get_tree conn in
            let curr_node = get_focused_window tree in
            order_tab conn format;%lwt
            let%lwt _ = focus_win conn curr_node.id in
            Lwt.return_unit
        | _ -> Lwt.return_unit )
    | _ -> Lwt.return_unit
  done

(*match !message_type with
  | "command" ->
    let%lwt outcomes = I3ipc.command conn payload in
    List.iter (fun outcome ->
      if not outcome.I3ipc.Reply.success then
        Format.fprintf fmt "ERROR: %s\n\n%!"
          (match outcome.I3ipc.Reply.error with Some s -> s | None -> "")
    ) outcomes;
    pp_list fmt (I3ipc.Reply.pp_command_outcome fmt) outcomes |> Lwt.return
  | "get_workspaces" ->
    let%lwt workspaces = I3ipc.get_workspaces conn in
    pp_list fmt (I3ipc.Reply.pp_workspace fmt) workspaces |> Lwt.return
  | "get_outputs" ->
    let%lwt outputs = I3ipc.get_outputs conn in
    pp_list fmt (I3ipc.Reply.pp_output fmt) outputs |> Lwt.return
  | "get_tree" ->
    let%lwt tree = I3ipc.get_tree conn in
    I3ipc.Reply.pp_node fmt tree |> Lwt.return
  | "get_marks" ->
    let%lwt marks = I3ipc.get_marks conn in
    pp_list fmt (Format.pp_print_string fmt) marks |> Lwt.return
  | "get_bar_ids" ->
    let%lwt bar_ids = I3ipc.get_bar_ids conn in
    pp_list fmt (Format.pp_print_string fmt) bar_ids |> Lwt.return
  | "get_bar_config" ->
    let%lwt bar_cfg = I3ipc.get_bar_config conn payload in
    I3ipc.Reply.pp_bar_config fmt bar_cfg |> Lwt.return
  | "get_version" ->
    let%lwt version = I3ipc.get_version conn in
    I3ipc.Reply.pp_version fmt version |> Lwt.return
  | "get_binding_modes" ->
    let%lwt binding_modes = I3ipc.get_binding_modes conn in
    I3ipc.Reply.pp_binding_modes fmt binding_modes |> Lwt.return
  | "get_config" ->
    let%lwt config = I3ipc.get_config conn in
    I3ipc.Reply.pp_config fmt config |> Lwt.return
  | "send_tick" ->
    let%lwt tick = I3ipc.send_tick conn payload in
    Format.pp_print_bool fmt tick |> Lwt.return
  | _ -> Format.fprintf Format.err_formatter "Unsupported message type"; exit 1
*)
let () = Lwt_main.run main
