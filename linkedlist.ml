(* Node *)
type 'a node = {
  value: 'a;
  mutable next: 'a node option;
}

(* Create a node *)
let create_node value = { value; next = None }

(* Add nodes to linked list *)
let rec add_node node value =
  match node.next with
  | None -> node.next <- Some (create_node value)
  | Some next_node -> add_node next_node value

(* Iterate over nodes *)
let rec iter_nodes node f =
  f node.value;
  match node.next with
  | None -> ()
  | Some next_node -> iter_nodes next_node f

(* Find a node in a linked list *)
let rec find_node node value =
  if node.value = value then Some node
  else
    match node.next with
    | None -> None
    | Some next_node -> find_node next_node value

(* Remove a node from a linked list *)
let rec remove_node node value =
  if node.value = value then node.next
  else
    match node.next with
    | None -> None
    | Some next_node ->
      let new_next = remove_node next_node value in
      node.next <- new_next;
      Some node

(* Example create a linked list *)
let my_list = create_node "first element";;
add_node my_list "second element";;
add_node my_list "third element";;

(* Print all nodes *)
iter_nodes my_list (fun value -> print_endline value);;
