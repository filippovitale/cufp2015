(* ###################################################################### *)
(** * Proofs and Programs *)

(** [Inductive] is Coq's way of defining an algebraic datatype.  Its
    syntax is similar to OCaml's ([type]) or Haskell's ([data]). Here,
    we define [bool] as a simple algebraic datatype. *)

Module Bool.

Inductive bool : Type :=
| true : bool
| false : bool.

(** Exercise: Define a three-valued data type, representing ternary
    logic.  Here something can be true, false and unknown. *)

Inductive trivalue : Type :=
| one : trivalue
| two : trivalue
| three : trivalue.

(** We can write functions that operate on [bool]s by simple pattern
    matching, using the [match] keyword. *)

Definition negb (b:bool) : bool :=
  match b with
  | true => false
  | false => true
  end.

Check negb.
Compute (negb true).

Definition orb (b1 b2: bool) : bool :=
  match b1, b2 with
  | false, false => false
  | _, _ => true
  end.

Print orb.

Definition andb (b1 b2: bool) : bool :=
  if b1 then b2 else false.

Definition xorb (b1 b2 : bool) : bool :=
  match b1, b2 with
  | true, false => true
  | false, true => true
  | _, _ => false
  end.

(** New tactics
    -----------

    - [intros]: Introduce variables into the context, giving them
      names.

    - [simpl]: Simplify the goal.

    - [reflexivity]: Prove that some expression [x] is equal to itself. *)

Example andb_false : forall b, andb false b = false.
Proof.
  intros b1.
  simpl.
  reflexivity.
Qed.

(** Exercise: Prove this. *)
Theorem orb_true_l :
  forall b, orb true b = true.
Proof.
  intros b.
  simpl.
  reflexivity.
Qed.

(** New tactics
    -----------

    - [destruct]: Consider all possible constructors of an inductive
      data type, generating subgoals that need to be solved
      separately. *)

Lemma double_negation : forall b : bool, negb (negb b) = b.
Proof.
  intros b.
  simpl.
  destruct b.
  (* focus on: *)
  + (* case: b is true *)
    simpl.
    reflexivity.
  + (* case: b is false *)
    simpl.
    reflexivity.
Qed.

Theorem andb_commutative : forall b1 b2 : bool, andb b1 b2 = andb b2 b1.
Proof.
 intros b1 b2.
 destruct b1.
 + simpl.
   destruct b2.
   - simpl.
     reflexivity.
   - simpl.
     reflexivity.
 + destruct b2; simpl; reflexivity.
Qed.

(* we should use `+` `-` `*` consistently *)

(** Exercise: Show that false is an identity element for xor -- that
    is, [xor false b] is equal to [b] *)

Theorem xorb_false : forall b : bool, xorb false b = b.
Proof.
 intros b.
 destruct b.
 + simpl.
   reflexivity.
 + simpl; reflexivity.
Qed.

(** New tactics
    -----------

    - [rewrite]: Replace one side of an equation by the other.

    - [apply]: Suppose that the current goal is [Q]. If [H : Q], then
      [apply H] solves the goal. If [H : P -> Q], then [apply H]
      replaces [Q] by [P] in the goal. If [H] has multiple hypotheses,
      [H : P1 -> P2 -> ... -> Pn -> Q], then [apply H] generates one
      subgoal for each [Pi]. *)

Theorem rewrite_example : forall b1 b2 b3 b4 : bool,
  b1 = b4 ->
  b2 = b3 ->
  andb b1 b2 = andb b3 b4.
Proof.
  intros b1 b2 b3 b4 E14 E23.
  rewrite <- E14.
  rewrite <- E23.
  apply andb_commutative.
Qed.

(** Exercise: Show that if [b1 = b2] then [xorb b1 b2] is equal to
    [false] *)

Theorem xorb_same : forall b1 b2 : bool, b1 = b2 -> xorb b1 b2 = false.
Proof.
  intros b1 b2 E.
  rewrite E.
(*
  destruct b2.
  + simpl.
    reflexivity.
  + simpl.
    reflexivity.
*)
  destruct b2; reflexivity.
Qed.

End Bool.

Module Nat.

(* ###################################################################### *)
(** * Numbers and induction *)

Inductive nat : Type :=
| O : nat
| S : nat -> nat.

Check (S (S O)). (* [C-c C-a C-c] in Proof General *)

Fixpoint plus (n m : nat) : nat :=
  match n with
  | O => m
  | S n' => S (plus n' m)
  end.

Fixpoint mult (n m : nat) : nat :=
  match n with
  | O => O
  | S n' => plus m (mult n' m)
  end.

(** Exercise: Define exponentiation *)

Fixpoint expo (n m : nat) : nat :=
  match m with
  | O    => (S O)
  | S m' => mult n (expo n m')
  end.

  Notation "x + y" := (plus x y) (at level 50, left associativity).

  Notation "x * y" := (mult x y) (at level 40, left associativity).

  Notation "x ^ y" := (expo x y) (at level 30, right associativity).

  Compute ( (S (S O)) + (S (S O)) ).
  Compute ( (S (S O)) * (S (S O)) ).
  Compute ( (S (S O)) ^ (S (S O)) ).

Lemma plus_0_l: forall n : nat, O + n = n.
Proof.
  intros n.
  simpl.
  reflexivity.
Qed.

(**
    New tactic
    ----------

    - [induction]: Argue by induction. It works as [destruct], but
    additionally giving us an inductive hypothesis in the inductive
    case. *)

Lemma plus_O_r: forall n : nat, n + O = n.
Proof.
  intros n.
  simpl.
  (* instead of: destruct n. *)
  induction n as [| n' IH].
  (* IH ==== Inductive Hypotesis *)
(*
  + simpl.
    reflexivity.

  reflexivity *includes* simpl
*)
  + reflexivity.
  + simpl.
    rewrite IH.
    reflexivity.
Qed.

Print plus_O_r.

Theorem plus_assoc: forall m n o, m + (n + o) = (m + n) + o.
Proof. 
  intros m n o.
  induction m.
  + simpl.
    reflexivity.
  + simpl.
    rewrite IHm.
    reflexivity.
 Qed.

(** Take-home exericse: Try to do induction on [n] and [o] in the
    above proof, and see where it fails. *)

(** Exercise: Show that [n + S m] is equal to [S (n + m)]. *)

Lemma plus_S_r : forall n m, n + S m = S (n + m).
Proof.
  intros n m.
  simpl.
  induction n as [| n' IH].
  + simpl.
    reflexivity.
  + simpl.
    rewrite IH.
    reflexivity.
Qed.

(** Exercise: Show that plus is commutative. *)
(** Hint: Look at our earlier lemmas. *)

Lemma plus_comm : forall n m, n + m = m + n.
Proof.
  intros n m.
  induction m.
  + simpl.
    apply plus_O_r.
  + simpl.
    rewrite <- IHm.
   apply plus_S_r.
Qed.

(*
  To search the right lemma: SearchAbout 

  e.g. SearchAbout (_ + S = _).
*)

(** Additional take-home exercises: Show that mult has an identity [S
    O], a annihilator [O] and associative, commutative and
    distributive properties. *)

Fixpoint minus (m n : nat) : nat :=
  match m, n with
  | O, _ => m
  | _, O => m
  | S m', S n' => minus m' n'
  end.

Notation "x - y" := (minus x y) (at level 50, left associativity).

Fixpoint ble_nat (m n : nat) : bool :=
  match m, n with
  | O, _ => true
  | _, O => false
  | S n', S m' => ble_nat n' m'
  end.

Fail Fixpoint div (m n: nat) : nat :=
  match n with
  | O => O
  | S n' => if ble_nat n m then S (div (m - n) n) else O
  end.

Fixpoint div (m n: nat) : nat :=
  match n with
  | O => O
  | S n' => match m with
            | O => O
            | S m' => S (div (S m' - S n') (S n'))
            end
  end.
Fixpoint beq_nat (m n : nat) : bool :=
  match m, n with
  | O, O => true
  | S m', S n' => beq_nat m' n'
  | _, _ => false
  end.

Definition max (m n : nat) : nat :=
  if ble_nat m n then n else m.

(** New tactic
    ----------

    - [clear]: Remove hypotheses from the context (needed here to
      simplify our IH). *)

(* revert is the opposite of intros *)

Lemma beq_nat_eq :
  forall m n, m = n -> beq_nat m n = true.
Proof.
  intros m n e. rewrite e. clear m e.
  induction n as [|n IH].
  - reflexivity.
  - simpl. apply IH.
Qed.

(** New tactics
    -----------

    - [discriminate]: Looks for an equation between terms starting
      with different constructors, and solves the current goal.

    - [revert]: The opposite of [intros]; removes variables and
      hypotheses from the context, putting them back in the goal. *)

Lemma eq_beq_nat :
  forall m n, beq_nat m n = true -> m = n.
Proof.
(* 1st try
  intros m n e.
  induction m.
  + destruct n.
      - reflexivity.
      - simpl in e.
         discriminate.
  + rewrite IHm
*)
  intros m n e.
  revert n e.
  induction m.
  + intros n e.
      destruct n.
      - reflexivity.
      - simpl in e.
         discriminate.
  + intros n H.
      destruct n.
      - simpl in H.
          discriminate.
      - simpl in H.
        apply IHm in H.
        rewrite H.
        reflexivity.
Qed.

Lemma plus_eq_0 : forall n m,  n + m = O -> n = O.
Proof.
(*  SearchAbout (_ + (S _) = _). 
  intros n.
  induction n.
  + reflexivity.
  + intros m H.
      destruct n.
*)

  intros n.
  induction n.
  + reflexivity.
  + intros m H.
      (* inversion H. *)
       simpl in H.
       discriminate.

Qed.

End Nat.


(** Useful notation... *)

Compute (S (S O)).
Compute (S (S O) + S O).

(* ###################################################################### *)
(** * Lists *)

Module List.

(** Here's a polymorphic definition of a [list] type in Coq: *)

Inductive list (T : Type) :=
| nil : list T
| cons : T -> list T -> list T.

(* Explicit polymorphism. *)
Definition singleton_list (T : Type) (x : T) :=
  cons T x (nil T).

(* Inferred (but still a bit explicit) *)
Definition singleton_list' (T : Type) (x : T) :=
  cons _ x (nil _).

(* Implicit, inferred all the time *)
Arguments nil {T}.
Arguments cons {T} _ _.
Definition singleton_list'' {T} (x : T) :=
  cons x nil.

Check (singleton_list'' 3).
Check (@singleton_list'' nat).

(* @ turn off implicit arguments *)

Check @singleton_list''.
Check @nil.

Notation "h :: t" := (cons h t) (at level 60, right associativity).
Notation "[ ]" := (nil).
Notation "[ x ; .. ; y ]" := (cons x .. (cons y nil) ..).

(* Exercise: Define "snoc", which adds an element to the end of a list. *)

Fixpoint snoc {T} (l : list T) (x : T) : list T :=
  match l with
  | []       => [x]
  | x' :: l' => x' :: snoc l' x
  end.

Fixpoint app {T} (l1 l2 : list T) : list T :=
  match l1 with
  | [] => l2
  | h :: l1' => h :: (app l1' l2)
  end.

Notation "l1 ++ l2" := (app l1 l2) (at level 60, right associativity).

Lemma app_assoc :
  forall T (l1 l2 l3 : list T),
    l1 ++ (l2 ++ l3) = (l1 ++ l2) ++ l3.
Proof.
(*  intros T l x.
  induction l as [|x' l IH]; simpl.
  - reflexivity.
  - rewrite IH. reflexivity.
Qed.
*)
Admitted.

Lemma snoc_app :
  forall T (l : list T) (x : T),
    snoc l x = l ++ [x].
Proof.
  (* Fill in here *)
Admitted.

End List.

(** Lists, of course, are also defined in the standard library. *)

Require Import Coq.Lists.List.
Import ListNotations.

(** Notice that the definition of rev (list reversal) given in the
    standard library runs in quadratic time. *)

Print rev. (* [C-c C-a C-p] in Proof General *)

(** This is a tail-recursive equivalent that runs in linear time. *)

Fixpoint tr_rev_aux {T} (l acc : list T) : list T :=
  match l with
  | [] => acc
  | x :: l => tr_rev_aux l (x :: acc)
  end.

Definition tr_rev {T} (l: list T) := tr_rev_aux l [].

(** New Tactic
    ----------

    - [unfold]: Calling [unfold foo] expands the definition of [foo]
      in the goal.
*)

Lemma tr_rev_aux_correct :
  forall T (l1 l2 : list T),
    tr_rev_aux l1 l2  = rev l1 ++ l2.
Proof.
  intros T l1 l2.
  revert l2.
  induction l1 as [|x1 l1 IH]; simpl.
  - intros l2. reflexivity.
  - intros l2. rewrite IH.
    SearchAbout (_ ++ _ ++ _).
    rewrite <- app_assoc.
    simpl.
    reflexivity.
Qed.

Lemma tr_rev_correct :
  forall T (l : list T),
    tr_rev l = rev l.
Proof.
  intros T l.
  unfold tr_rev.
  rewrite tr_new_aux_correct.
  simpl.
  SearchAbout (_ ++ [])
(*
  induction l as [|x l IH]. simpl.
  - reflexivity.
  - .
*)
  apply app_nil_r.
Qed.

(* ###################################################################### *)
(** * Dependently Typed Programming *)

Definition stack := list.

Definition push {T} (x:T) (s : stack T) : stack T  := x :: s.

(* Definition pop {T} (s : stack T) : T * stack T := *)

(* Length-Indexed Stacks *)

(* Exercise: Write a snoc function to add an element to the bottom of
   an indexed stack. Do not use the combine function (in this case, it
   will make life difficult.) *)
