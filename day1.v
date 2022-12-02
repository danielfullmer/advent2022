Require Import Ascii.
Require Import Nat.
Require Import BinNat.
Require Import Decimal.
Require Import DecimalString.
Require Import List.
Require Import String.
Import ListNotations.
Local Open Scope string_scope.

Definition str : string := "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000".

Fixpoint split_on (sep: ascii) (accum : string) (s : string) {struct s} : list string :=
    match s with
    | EmptyString => [accum]
    | String h t => match Ascii.compare h sep with
        | Eq => [accum] ++ split_on sep "" t
        | _ => split_on sep (accum ++ String h EmptyString) t
        end
    end.

Fixpoint split_on_string (sep: string) (accum : list string) (s : list string) {struct s} : list (list string) :=
    match s with
    | [] => [accum]
    | h::t => match String.compare h sep with
        | Eq => [accum] ++ split_on_string sep [] t
        | _ => split_on_string sep (accum ++ [h]) t
        end
    end.

Definition newline : ascii := "010".

Definition strings_to_counts (s : string) : list (list N) :=
    map (fun x => map (fun y => match NilEmpty.uint_of_string y with
                                | Some z => N.of_uint z
                                | None => 0%N
                                end)
                        x)
        (split_on_string "" [] (split_on newline "" s)).

Definition Nlist_sum l := fold_right Nplus 0%N l.
Definition Nlist_max l := fold_right N.max 0%N l.

Definition calories s := Nlist_max (map Nlist_sum (strings_to_counts s)).
