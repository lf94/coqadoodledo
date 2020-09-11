
type bool =
| True
| False

type nat =
| O
| S of nat

(** val add : nat -> nat -> nat **)

let rec add n m =
  match n with
  | O -> m
  | S p -> S (add p m)

(** val sub : nat -> nat -> nat **)

let rec sub n m =
  match n with
  | O -> n
  | S k -> (match m with
            | O -> n
            | S l -> sub k l)

type ascii =
| Ascii of bool * bool * bool * bool * bool * bool * bool * bool

type string =
| EmptyString
| String of ascii * string

(** val length : string -> nat **)

let rec length = function
| EmptyString -> O
| String (_, s') -> S (length s')

(** val substring : nat -> nat -> string -> string **)

let rec substring n m s =
  match n with
  | O ->
    (match m with
     | O -> EmptyString
     | S m' ->
       (match s with
        | EmptyString -> s
        | String (c, s') -> String (c, (substring O m' s'))))
  | S n' ->
    (match s with
     | EmptyString -> s
     | String (_, s') -> substring n' m s')

type 'a option =
| None
| Some of 'a

(** val _position_of_matching_parenthesis :
    string -> nat -> nat -> nat option **)

let rec _position_of_matching_parenthesis str position count = match count with
| O -> Some position
| S _ ->
  (match str with
   | EmptyString -> None
   | String (c, str') ->
     let Ascii (b, b0, b1, b2, b3, b4, b5, b6) = c in
     (match b with
      | True ->
        (match b0 with
         | True ->
           _position_of_matching_parenthesis str' (add position (S O)) count
         | False ->
           (match b1 with
            | True ->
              _position_of_matching_parenthesis str' (add position (S O))
                count
            | False ->
              (match b2 with
               | True ->
                 (match b3 with
                  | True ->
                    _position_of_matching_parenthesis str'
                      (add position (S O)) count
                  | False ->
                    (match b4 with
                     | True ->
                       (match b5 with
                        | True ->
                          _position_of_matching_parenthesis str'
                            (add position (S O)) count
                        | False ->
                          (match b6 with
                           | True ->
                             _position_of_matching_parenthesis str'
                               (add position (S O)) count
                           | False ->
                             _position_of_matching_parenthesis str'
                               (add position (S O)) (sub count (S O))))
                     | False ->
                       _position_of_matching_parenthesis str'
                         (add position (S O)) count))
               | False ->
                 _position_of_matching_parenthesis str' (add position (S O))
                   count)))
      | False ->
        (match b0 with
         | True ->
           _position_of_matching_parenthesis str' (add position (S O)) count
         | False ->
           (match b1 with
            | True ->
              _position_of_matching_parenthesis str' (add position (S O))
                count
            | False ->
              (match b2 with
               | True ->
                 (match b3 with
                  | True ->
                    _position_of_matching_parenthesis str'
                      (add position (S O)) count
                  | False ->
                    (match b4 with
                     | True ->
                       (match b5 with
                        | True ->
                          _position_of_matching_parenthesis str'
                            (add position (S O)) count
                        | False ->
                          (match b6 with
                           | True ->
                             _position_of_matching_parenthesis str'
                               (add position (S O)) count
                           | False ->
                             _position_of_matching_parenthesis str'
                               (add position (S O)) (add count (S O))))
                     | False ->
                       _position_of_matching_parenthesis str'
                         (add position (S O)) count))
               | False ->
                 _position_of_matching_parenthesis str' (add position (S O))
                   count)))))

(** val position_of_matching_parenthesis : string -> nat -> nat option **)

let position_of_matching_parenthesis str position =
  _position_of_matching_parenthesis
    (substring (add position (S O)) (length str) str) position (S O)
