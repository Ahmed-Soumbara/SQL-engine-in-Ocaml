open Projet

(* === TABLE DEFINITIONS === *)
let test = {cols = [ ("id", (TInt, false)); ("name", (TText, true)); ("age", (TInt, false)) ];
    rows = [ [VInt 1; VText "Alice"; VInt 30];
             [VInt 2; VText "Bob"; VInt 30];
             [VInt 3; VText "Fred"; VInt 23] ]};;

let test_rule1 = {cols = [ ("id", (TInt, false)); ("name", (TText, true));
                            ("age", (TInt, false)); ("email", (TText, true)) ];
    rows = [ [VInt 1; VText "Alice"; VInt 30];
             [VInt 2; VText "Bob"; VInt 30];
             [VInt 3; VText "Fred"; VInt 23] ]};;

let test_rule_2_3 = {cols = [ ("id", (TInt, false)); ("name", (TText, true)); ("age", (TInt, false)) ];
    rows = [ [VNull; VText "Alice"; VInt 30];
             [VInt 2; VText "Bob"; VInt 30];
             [VInt 3; VText "Fred"; VText "Bob"] ]};;

let test_rule_4 = {cols = [ ("id", (TInt, false)); ("name", (TText, true)); ("id", (TInt, false)) ];
    rows = [ [VInt 1; VText "Alice"; VInt 30];
             [VInt 2; VText "Bob"; VInt 30];
             [VInt 3; VText "Fred"; VInt 23] ]};;

let insert_test = insert test [VInt 69; VText "Jeff"; VInt 21];;

let tb1 = {cols = [("a", (TInt, false)); ("b", (TText, true))];
           rows = [[VInt 1; VText "Alice"]; [VInt 2; VText "Bob"]]};;

let tb2 = {cols = [("c", (TInt, true))];
           rows = [[VInt 99]; [VInt 11]]};;

let p = prod tb1 tb2;;

let student_table = {
  cols = [("student", (TText, false)); ("course", (TText, false));
          ("teacher", (TText, false)); ("room", (TText, false))];
  rows = [
    [VText "Alice"; VText "Math";    VText "Mr.Smith"; VText "A101"];
    [VText "Alice"; VText "Physics"; VText "Mr.Jones"; VText "B202"];
    [VText "Bob";   VText "Math";    VText "Mr.Smith"; VText "A101"];
    [VText "Bob";   VText "Physics"; VText "Mr.Jones"; VText "B202"];
    [VText "Carol"; VText "Math";    VText "Mr.Smith"; VText "C303"];
  ]};;

let test1 = {
  cols = [("emp_id", (TInt, false)); ("dept_id", (TInt, false)); ("dept_name", (TText, false))];
  rows = [
    [VInt 1; VInt 10; VText "HR"];
    [VInt 2; VInt 20; VText "IT"];
    [VInt 3; VInt 10; VText "HR"];
    [VInt 4; VInt 30; VText "Finance"];
  ]};;

let threenf_table = {
  cols = [("student_id", (TInt, false)); ("name", (TText, false)); ("email", (TText, false))];
  rows = [
    [VInt 1; VText "Alice"; VText "alice@mail.com"];
    [VInt 2; VText "Bob";   VText "bob@mail.com"];
    [VInt 3; VText "Alice"; VText "alice2@mail.com"];
    [VInt 4; VText "Carol"; VText "carol@mail.com"];
  ]};;

let twonf_table = {
  cols = [("order_id", (TInt, false)); ("customer_id", (TInt, false)); ("customer_name", (TText, false))];
  rows = [
    [VInt 1; VInt 101; VText "Alice"];
    [VInt 2; VInt 102; VText "Bob"];
    [VInt 3; VInt 101; VText "Alice"];
    [VInt 4; VInt 103; VText "Carol"];
  ]};;

let onenf_table = {
  cols = [("student_id", (TInt, false)); ("course_id", (TInt, false));
          ("grade", (TInt, false)); ("course_name", (TText, false))];
  rows = [
    [VInt 1; VInt 10; VInt 90; VText "Math"];
    [VInt 1; VInt 20; VInt 90; VText "Physics"];
    [VInt 2; VInt 10; VInt 90; VText "Math"];
    [VInt 2; VInt 20; VInt 90; VText "Physics"];
    [VInt 3; VInt 10; VInt 90; VText "Math"];
    [VInt 3; VInt 20; VInt 90; VText "Physics"];
    [VInt 4; VInt 10; VInt 90; VText "Math"];
    [VInt 4; VInt 20; VInt 90; VText "Physics"];
  ]};;

(* === CHECK TABLE TESTS === *)
let () =
    Printf.printf "=== CHECK TABLE TESTS ===\n";
    Printf.printf "\nTesting a valid table\n";
    print_table test;
    Printf.printf "check_table test (expected true): %b\n" (check_table test);
    Printf.printf "\ntesting a table that violates rule 1 \n";
    print_table test_rule1;
    Printf.printf "check_table test_rule1 (expected false): %b\n" (check_table test_rule1);
    Printf.printf "\nA table that violates Rule 2 and 3\n";
    print_table test_rule_2_3;
    Printf.printf "check_table test_rule_2_3 (expected false): %b\n" (check_table test_rule_2_3);
    Printf.printf "\nThat violates Rule 4\n";
    print_table test_rule_4;
    Printf.printf "check_table test_rule_4 (expected false): %b\n" (check_table test_rule_4);;

(* === INSERT TEST === *)
let () =
    
    Printf.printf "\n=== INSERT TEST ===\n";
    Printf.printf "table before insertion :\n";
    print_table test ;
    Printf.printf "\ntable after insertion :\n";
    print_table insert_test;
    Printf.printf "\ninsert test passed: %b\n" (check_table insert_test);;

(* === PROD TEST === *)
let () =
    Printf.printf "\n=== PROD TEST ===\n";
    print_table tb1;
    Printf.printf " \n\n";
    print_table tb2;
    Printf.printf "\nresulting table : \n";
    print_table p;
    Printf.printf "prod rows count (expected 4): %d\n" (List.length p.rows);;

(* === PROJECTION TEST === *)
let () =
    Printf.printf "\n=== PROJECTION TEST ===\n";
    Printf.printf "original table\n";
    print_table test ;
    Printf.printf "\nprojection on id\n";
    print_table (projection test ["id"]);
    Printf.printf "\nprojection on id and name\n";
    print_table (projection test ["id";"name"]);
    Printf.printf "\non id and age\n";
    print_table (projection test ["id";"age"]);
    Printf.printf "\non age and name\n";
    print_table (projection test ["age";"name"]);;

(* === RESTRICT TEST === *)
let () =
    Printf.printf "\n=== RESTRICT TEST ===\n";
    Printf.printf "restricting on where age > 25\n";
    print_table (restrict test (fun row ->
        match List.nth row 2 with
        | VInt n -> n > 25
        | _ -> false));;

(* === COMPUTE DEPS TEST === *)
let () =
    Printf.printf "\n=== COMPUTE_ELEMENTARY_DEPS TEST ===\n";
    print_table student_table ; 
    let deps = compute_elementary_deps student_table in
    Printf.printf "student_table elementary deps count: %d\n" (List.length deps);;

(* === NORMALIZATION TESTS === *)
let () =
    Printf.printf "\n=== NORMALIZATION TESTS ===\n";
    print_table threenf_table ;
    Printf.printf "normalization threenf_table (expected 3): %d\n" (normalization_level threenf_table);
    Printf.printf "\n\n";
    print_table twonf_table;
    Printf.printf "normalization twonf_table (expected 2): %d\n" (normalization_level twonf_table);
    Printf.printf "\n\n";
    print_table test1 ;
    Printf.printf "normalization test1/transitive (expected 2): %d\n" (normalization_level test1);
    Printf.printf "\n\n";
    print_table onenf_table;
    Printf.printf "normalization onenf_table (expected 1): %d\n" (normalization_level onenf_table);;