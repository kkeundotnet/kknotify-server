type 'a t = {old_stack: 'a list; new_stack: 'a list}

exception Empty

let empty = {old_stack= []; new_stack= []}

let add e x = {x with new_stack= e :: x.new_stack}

let push = add

let take x =
  match x.new_stack with
  | [] -> (
    match List.rev x.old_stack with
    | [] -> raise Empty
    | e :: tl -> (e, {new_stack= tl; old_stack= []}) )
  | e :: tl -> (e, {x with new_stack= tl})

let pop = take

let peek x =
  let e, _x = take x in
  e

let top = peek

let is_empty {old_stack; new_stack} =
  match (old_stack, new_stack) with [], [] -> true | _, _ -> false

let length {old_stack; new_stack} =
  List.length old_stack + List.length new_stack

let iter {old_stack; new_stack} ~f =
  List.iter f old_stack ;
  List.iter f (List.rev new_stack)

let fold {old_stack; new_stack} ~init ~f =
  let rec list_fold x ~init ~f =
    match x with [] -> init | hd :: tl -> list_fold tl ~init:(f hd init) ~f
  in
  let acc = list_fold old_stack ~init ~f in
  list_fold (List.rev new_stack) ~init:acc ~f
