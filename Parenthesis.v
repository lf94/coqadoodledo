Require Import Coq.Strings.String.
Local Open Scope string_scope.

Require Import Ascii.
Require Import PeanoNat.

Inductive Option {A : Type} : Type :=
| None
| Some (a : A).

Fixpoint _position_of_matching_parenthesis (str : string) (position : nat) (count : nat) : Option :=
 match count with
 | 0 => Some position
 | _ => match str with
   | EmptyString => None
   | String c str' => match c with
     | "("%char => _position_of_matching_parenthesis str' (position + 1) (count + 1)
     | ")"%char => _position_of_matching_parenthesis str' (position + 1) (count - 1)
     | _ => _position_of_matching_parenthesis str' (position + 1) count
     end
   end
 end.

Definition position_of_matching_parenthesis (str : string) (position : nat) : Option :=
_position_of_matching_parenthesis (substring (position + 1) (length str) str) position 1.

Theorem any_empty_position_is_none : forall p : nat,
position_of_matching_parenthesis EmptyString p = None.
Proof.
intros.
unfold position_of_matching_parenthesis.
destruct p.
- simpl. reflexivity.
- simpl. reflexivity. 
Qed.

Theorem not_all_zeroth_are_valid : forall a : nat, forall s : string, exists s,
position_of_matching_parenthesis s 0 = None.
Proof.
intros.
exists EmptyString.
unfold position_of_matching_parenthesis.
simpl.
reflexivity.
Qed.
