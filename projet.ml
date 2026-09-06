(* Le type [dbtype] défini les différents types possiblement présents
   en base dans notre cadre.

  Nous n'aurons ici besoin que de deux types primitifs.

  - les entiers représentés ici par TInt
  - les textes représentés ici par TText
 *)
type dbtype =
  | TInt  (* type des entrées entières *)
  | TText (* type des entrées textes   *)
;; 

(* Le type [coltype] est le type représentant un champ dans une table
   de notre système.

   Il est composé d'un couple comprenant le type des valeurs présentes
   dans ce champ d'une part et d'un booléen exprimant la possibilité
   (dans le cas où le booléen est à [true]) ou non pour ce champs
   d'adopter la valeur [NULL] *)
type coltype = dbtype * bool ;;

(* Le type [dbvalue] est le type des *VALEURS* présentes en base.

  Nous aurons besoin ici de trois types de valeurs.

  - les valeurs entières munies de leurs valeurs
  - les valeurs textuelles munies de leurs valeurs
  - la valeur null qui pourras être indifféremment considérée de type
   [TInt] et [TText].  *)
type dbvalue =
  | VInt of int     (* valeurs entières   *)
  | VText of string (* valeurs textuelles *)
  | VNull           (* la valeur NULL *)
;;

(* Le schéma d'une table est une liste de couple dont le premier
   élément est le nom du champs et le second est le type du champs de
   type [coltype] *)
type schema = (string*coltype) list ;;


(* Une ligne d'une table est une liste de valeurs.
 *)
type row = dbvalue list ;;

(* Une [table] est la donnée d'un schéma et d'une liste de lignes *)
type table = { cols : schema; rows : row list } ;;

(* Le type [fd] représente le type des dépendances fonctionnelles
   d'une table.

   Il est composé d'un couple (lhs,rhs) dont chacun des deux membres
   est une liste de nom de champs.

   La dépendance (lhs,rhs) représente bien évidement la dépendance lhs -> rhs. 
 *)
type fd = (string list) * (string list) ;;



(* [check_table t] vérifie que la table [t] est valide.
 *)
let check_table tbl =

   (* 1st rule *)
   let first_rule tbl = 
      let col_length = List.length tbl.cols in 
      let row_checker row n =  if(List.length row <> n) then false
                               else true in 

      List.for_all (fun row -> row_checker row col_length) tbl.rows  
   in
   
   (* rule 2 and 3  *)
   let second_third_rule tbl = 
      let compare_val_col value coltype =
      match value, coltype with
      | VInt _,  (_, (TInt,  _))    -> true
      | VText _, (_, (TText, _))    -> true
      | VNull,   (_, (_, true))     -> true   
      | _,       _             -> false
      in
      let compare_row_val_type row col = List.for_all2 (fun value col -> compare_val_col value col) row col 
      in 
      
      List.for_all (fun row -> compare_row_val_type row tbl.cols) tbl.rows 
   in

   (* rule 4 *)
   (* [has_no_duplicates lst] returns true if theres no duplicate elements in lst 
   , false otherwise *)
   let rec has_no_duplicates lst =
    match lst with
    | [] -> true
    | x :: rest -> not (List.mem x rest) && has_no_duplicates rest
   in
   (* [forth_rule tbl] applies has_no_duplicates on tbl.cols  *)
   let fourth_rule tbl =
    let col_names = List.map (fun (name, _) -> name) tbl.cols in
    has_no_duplicates col_names
   in

   first_rule tbl && second_third_rule tbl && fourth_rule tbl
   ;;


(* [insert t r] insère si possible la ligne [r] dans la table [tbl].
*)
let insert tbl row = 
   let new_tbl = {cols = tbl.cols ; rows = tbl.rows@[row]} in
   if (check_table new_tbl) then new_tbl else failwith "given row is invalid \n"
   ;;



(* [prod tbl1 tbl2] effectue le produit cartésien des tables [tbl1] et [tbl2]
 *)                   
let prod tbl1 tbl2 = 
   let new_cols = tbl1.cols @ tbl2.cols in 

   let rec f cst bis acc  = 
      match bis with 
      |[] -> acc
      |l::ls -> let acc' = [cst@l]@acc in 
                  f cst ls acc'
      in

   let new_rows = List.concat_map (fun l -> f l tbl2.rows []) tbl1.rows in
   {cols = new_cols ; rows = new_rows}
   
   ;; 

(* [find_row_row index field counter cols] fonction auxiliere qui retourne l'indice
dand la liste des rows selon le field 
utilisé dans : projection , compute_deps*)
let rec  find_row_index field counter cols =
   match cols with 
   |[]-> failwith "field not found in cols"
   |(name,_)::rest_cols -> if(name = field) then counter  
                           else find_row_index field (counter +1) rest_cols
;;

(* [projection tbl fields] effectue la projection suivant la liste de
   champs [fields] de la table [tbl] *)
let projection tbl fields = 
   
   let find_col field = List.find (fun (name,(_,_))-> name = field) tbl.cols
   in 
   
   let new_cols = List.map (fun field -> find_col field) fields in 

   let rec make_row acc field row = 
      match field with
      |[]-> acc
      |curr::rest-> let acc' = acc@[List.nth row (find_row_index curr 0 tbl.cols)]in
                     make_row acc' rest row 
   in 
   
   let new_rows = List.map (fun row -> make_row [] fields row) tbl.rows
   in 
   
   {cols = new_cols ; rows = new_rows}
   
;;



(* [restrict tbl test] effectue la restriction des données présentes
   dans la table [tbl] en accord avec la fonction [test]. On ne garde
   dans le résultat que les lignes pour lesquelles [test] retourne
   [true].  *)
let restrict tbl f = 
   {cols = tbl.cols ; rows = List.filter f tbl.rows}
   ;;

(* (*test de restrict  
select * where age >25 *)

restrict test (fun row -> 
    match List.nth row 2 with  (* age is at index 2 *)
    | VInt n -> n > 25
    | _ -> false
);; *)

(*renvoi la liste des sous liste d'une lst*)
let rec subsets lst =
 match lst with
 | [] -> [[]]  
 | x :: rest ->
     let subset_of_rest = subsets rest in
     subset_of_rest@List.map (fun element_of_list -> x :: element_of_list) subset_of_rest
;;

(* retourne : [[]; ["c"]; ["b"]; ["b"; "c"]; ["a"]; ["a"; "c"]; ["a"; "b"];
 ["a"; "b"; "c"]] *)
subsets ["a";"b"; "c"];;


(* [is strict_subset a b] returns true if a ∈ b, false otherwise *)
let is_strict_subset a b =
    List.length a < List.length b &&
    List.for_all (fun x -> List.mem x b) a
;;


(* [compute_deps tbl] retourne TOUTES les dépendances fonctionnelles
   trouvées en étudiant les données présentes dans [tbl] *)
let compute_deps tbl = 
   let col_names = List.map (fun (name, _) -> name) tbl.cols in

   let subset = subsets col_names in

   (* returns the row of values from restricing on fields *)
   let extract_values_from_row fields cols row =
      List.map (fun f -> List.nth row (find_row_index f 0 cols)) fields
   in
   
   (* checking if lhs->rhs is true *)
   let check_fd tbl  lhs rhs = 
      let proj = projection tbl (lhs @ rhs) in 

      (* lst des valeur de lhs  *)

      (* [dedup lst] gets rid of duplicates *)
      let dedup lst = List.sort_uniq compare lst in 
      let lhs_vals = dedup (List.map (fun row -> extract_values_from_row lhs proj.cols row) proj.rows)
      in

      let rec check_values values = 
         match values with 
         |[] -> true
         |v::vs -> 
            (* restriction de rows where lhs = v *)
            let restriction = restrict proj (fun row -> 
                         extract_values_from_row  lhs proj.cols row = v) in
            
            (* l'ensemble des valeur rhs sur la restriction *)
            let rhs_vals = List.map (fun row 
            -> extract_values_from_row  rhs proj.cols row) restriction.rows ;
            in

            (* on verifie si tous les valeur rhs sont egaux *)
            let are_rhs_equal = 
               match rhs_vals with 
               |[] -> true 
               |x::xs -> List.for_all (fun y-> y=x) xs
            in

            are_rhs_equal && check_values vs
      in
   
   check_values lhs_vals 
   in
   
   (* for every lhs rhs *)

   let all_pairs lst =
      List.concat (
        List.map (fun x ->
          List.map (fun y -> (x, y)) lst
       ) lst)

   in 

   
   
   let all_possible_combinations = all_pairs subset in 

   let all_pairs_no_empty = List.filter (fun (lhs, rhs) -> lhs <> [] && rhs <> []) all_possible_combinations in 

   let all_fds = List.filter (fun (lhs, rhs) -> 
             lhs <> rhs 
             && not (List.exists (fun col -> List.mem col lhs) rhs)
             && check_fd tbl lhs rhs
         ) all_pairs_no_empty
   in

   (* check_fd_union fd for every fd X->YZ returns false if X->Y and X->Z *)
   let check_fd_union (lhs,rhs) =   
      if(List.length rhs < 2) then false 
      else 
         match rhs with 
         |[]->false
         |_::_ -> List.for_all (fun y -> check_fd tbl lhs [y]) rhs 
   in

   List.filter (fun fd -> 
      not (check_fd_union fd)
            ) all_fds ;;



(* [compute_elementary_deps tbl] retourne TOUTES les dépendances
   fonctionnelles élémentaires trouvées en étudiant les données
   présentes dans [tbl] *)
let compute_elementary_deps tbl = 
   let all_fds = compute_deps tbl in
   
   (* check_atomic fd : returns true if rhs is atomic *)
   let check_atomic (lhs,rhs) = if (List.length rhs < 2 ) then true
                                 else false 
   in 
   
   (* check_redundant fd : returns true if fd is redundant *)
   (* (lhs,rhs) is redundant if there exists a fd (l,r) in all_fds 
   such that r = rhs and l is a strict subset of lhs *)
   let check_redundant (lhs, rhs) =
    List.exists (fun (smaller_lhs, same_rhs) ->
        same_rhs = rhs && is_strict_subset smaller_lhs lhs
    ) all_fds
    in
   

   List.filter (fun fd -> 
       (check_atomic fd)&&(not (check_redundant fd))
   )all_fds ;;
   


(* [normalization_level tbl] retourne le niveau de normalisation de
   [tbl] sous forme d'un entier.  *)
let normalization_level tbl = 
   
   let all_fds = compute_deps tbl in 
   let all_elem_fds = compute_elementary_deps tbl in 
   
   (* all tbls are 1nf by design*)


   (*compares 2 lists, returns true when l1 and l2 have the same length 
   and the same elements so l1 = l2   *)
   let same_elements l1 l2 =
    List.length l1 = List.length l2 &&
    List.for_all (fun x -> List.mem x l2) l1
   in

   let all_cols = List.map (fun (name, _) -> name) tbl.cols in 

   let merged_fds = List.map (fun lhs ->
    let all_rhs = List.concat (List.filter_map (fun (l, r) ->
        if l = lhs then Some r else None
    ) all_fds) in
    (lhs, all_rhs)
) (List.sort_uniq compare (List.map (fun (lhs,_) -> lhs) all_fds))
   in

   (* a lhs is a key if lhs @ rhs covers all columns *)
   let keys_fds = List.filter (fun (lhs, rhs) -> 
       same_elements (lhs @ rhs) all_cols
   ) merged_fds
   in
   let all_keys = List.map (fun (lhs, _) -> lhs) keys_fds in

   (* keep only minimal keys *)
   let keys = List.filter (fun key ->
       not (List.exists (fun other_key ->
           is_strict_subset other_key key
       ) all_keys)
   ) all_keys in 
   
   let att_not_in_keys = List.filter (fun lhs -> not (List.exists (fun key ->
                                                                List.mem lhs key)
                                                                 keys)
                                    ) all_cols 
   in 

   (* DEBUG 
   Printf.printf "keys: %d\n" (List.length keys);
   Printf.printf "att_not_in_keys: %d\n" (List.length att_not_in_keys);
   Printf.printf "elem_fds: %d\n" (List.length all_elem_fds);
   *)

   (* DEBUG *)
      (* print a string list *)
   let rec print_str_list lst =
       match lst with
       | [] -> ()
       | x :: rest -> Printf.printf "%s " x; print_str_list rest
   in

   (* print a list of string lists (keys) *)
   let rec print_keys_list lst =
       match lst with
       | [] -> ()
       | key :: rest ->
           Printf.printf "[";
           print_str_list key;
           Printf.printf "]\n";
           print_keys_list rest
   in

   (* print a list of fds *)
   let rec print_fds lst =
       match lst with
       | [] -> ()
       | (lhs, rhs) :: rest ->
           Printf.printf "[";
           print_str_list lhs;
           Printf.printf "] -> [";
           print_str_list rhs;
           Printf.printf "]\n";
           print_fds rest
   in

   (* then use them *)
   Printf.printf "keys:\n";
   print_keys_list keys;
   Printf.printf "att_not_in_keys:\n";
   print_str_list att_not_in_keys;
   Printf.printf "\n";
   Printf.printf "elem_fds:\n";
   print_fds all_elem_fds;

   (* get all strict subsets of a key *)
   let strict_subsets_of key = 
       List.filter (fun s -> is_strict_subset s key) (subsets key) 
   in

   (* does this subkey determine att in all_fds? *)
   let subkey_determines subkey att = 
       List.exists (fun (lhs, rhs) -> 
           lhs = subkey && List.mem att rhs
       ) all_fds 
   in

   (* does any strict subset of this key determine att? *)
   let key_partially_determines key att = 
       List.exists (fun subkey -> 
           subkey_determines subkey att
       ) (strict_subsets_of key) 
   in

   (* is att partially dependent on ANY key? *)
    (* [is_dependent_on_key att] returns true if dependent on a
      subset of keys  *)
   
   let is_dependent_on_subkey att = 
       List.exists (fun key -> 
           key_partially_determines key att
       ) keys 
   in
     
   
   (* [is_2nf atts] returns true when there is no att that is dependent
   on a subset of a key*)
   let is_2nf atts = 
      List.for_all (fun att -> not (is_dependent_on_subkey att) )
         atts in 

   let is_3nf_cond = List.for_all (fun (x, ak) ->
    match ak with
    | [a] -> List.mem x keys || 
              List.exists (fun key -> List.mem a key) keys
    | _ -> false
      )all_elem_fds
   in
   
   if not (is_2nf att_not_in_keys) then 1
    else if not (is_3nf_cond) then 2
    else 3
   
   ;;

   (* to print tables in test*)
   let print_table tbl =
    (* print column names *)
    let rec print_cols cols =
        match cols with
        | [] -> Printf.printf "|\n"
        | (name, _) :: rest ->
            Printf.printf "| %-10s " name;
            print_cols rest
    in
    (* print separator *)
    let rec print_sep cols =
        match cols with
        | [] -> Printf.printf "+\n"
        | _ :: rest ->
            Printf.printf "+-----------";
            print_sep rest
    in
    (* print one value *)
    let print_val v =
        match v with
        | VInt n  -> Printf.printf "| %-10d " n
        | VText s -> Printf.printf "| %-10s " s
        | VNull   -> Printf.printf "| %-10s " "NULL"
    in
    (* print one row *)
    let rec print_row row =
        match row with
        | [] -> Printf.printf "|\n"
        | v :: rest -> print_val v; print_row rest
    in
    (* print all rows *)
    let rec print_rows rows =
        match rows with
        | [] -> ()
        | row :: rest -> print_row row; print_rows rest
    in
    print_cols tbl.cols;
    print_sep tbl.cols;
    print_rows tbl.rows
;;