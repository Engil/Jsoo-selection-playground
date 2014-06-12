let saveSelection elt =
  try
    let sel = Dom_html.window##getSelection () in
    let range = sel##getRangeAt(0) in
    let pre_range = range##cloneRange () in
    pre_range##selectNodeContents(elt);
    pre_range##setEnd(range##startContainer, range##startOffset);
    let start = (pre_range##toString())##length in
    Printf.printf "st: %d ed: %d\n" start (range##toString())##length;
    (start, start + (range##toString())##length)
  with e -> print_endline @@ Printexc.to_string e; 1,0

exception Not_text

let get_length node =
  try
    (Js.Opt.get (Dom.CoerceTo.text node) (fun () -> raise Not_text ))##length
  with
  | Not_text -> 0

let restoreSelection containerEl (start, ends) =
  let charIndex = ref 0 in
  let range = Dom_html.document##createRange() in
  range##setStart(containerEl, 0);
  range##collapse(Js._true);
  let nodeStack = Stack.create () in
  let foundStart = ref false in
  let stop = ref false in
  let rec inner stack node =
    if not !stop then
      if node##nodeType = Dom.TEXT
      then
        begin
          let next_index = !charIndex + (get_length node) in
          if not !foundStart && (start >= !charIndex) && (start <= next_index) then
            begin
              range##setStart(node, start - !charIndex);
              foundStart := true
            end;
          if !foundStart && (ends >= !charIndex) && (ends <= next_index) then
            begin
              range##setEnd(node, ends - !charIndex);
              stop := true
            end;
          charIndex := next_index
        end
      else
        begin
          let max = node##childNodes##length in
          for i = 0 to (max - 1) do
            Stack.push (Js.Opt.get (node##childNodes##item (i)) (fun () -> assert false)) stack
          done;
        end;
      try inner stack (Stack.pop stack) with Stack.Empty -> ()
  in
  inner nodeStack containerEl;
  let sel = Dom_html.window##getSelection() in
  sel##removeAllRanges();
  sel##addRange(range)

let save = ref None

let doSave elt = save := Some (saveSelection elt)

let restoreSave elt = match !save with
  | Some save -> restoreSelection elt save
  | _ -> ()

module Html = Dom_html
let onload _ =
  let d = Html.document in
  let editor =   Js.Opt.get (d##getElementById (Js.string "editor"))
      (fun () -> assert false) in
  let body =   Js.Opt.get (d##getElementById (Js.string "edi-body"))
      (fun () -> assert false) in
  let but = Html.createInput ?_type:(Some (Js.string "submit")) d in
  but##value <- Js.string "save";
  but##onclick <- Html.handler
      (fun _ -> doSave editor; Js._true);
  Dom.appendChild body but;
  let but2 = Html.createInput ?_type:(Some (Js.string "submit")) d in
  but2##value <- Js.string "restore";
  but2##onclick <- Html.handler
      (fun _ -> restoreSave (editor :> Dom.node Js.t); Js._true);
  Dom.appendChild body but2;
  Js._false

let _ = Html.window##onload <- Html.handler onload
