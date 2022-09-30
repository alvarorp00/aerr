# Script that generates the SMT-LIB2 code corresponding to the teacher assignment problem

defmodule Subjects do
  import Enum

  # For each teacher i we have a propositional variable t_i, with the meaning
  # "The i-th teacher belongs to the resulting set"
  def selected_teacher(teacher), do: "t_#{teacher}"
  
  
  # For each pair (i, j), we have a variable a_i_j with the meaning
  # "The i-th teacher occupies the j-th place in the resulting set of selected teachers"
  def teacher_in_place(teacher, place), do: "a_#{teacher}_#{place}"
  
  # For each subject i, we have a variable c_i with the meaning
  # "The i-th is covered by some of the currently selected teachers"
  def is_covered(subject), do: "c_#{subject}"
  
    
  def declare_selected_teacher(teachers) do
    for t <- teachers do
      "(declare-const #{selected_teacher(t)} Bool)"
    end |> join("\n")
  end
  
  def declare_teacher_in_place(teachers, k) do
    for t <- teachers, pl <- 1..k do
      "(declare-const #{teacher_in_place(t, pl)} Bool)"
    end |> join("\n")
  end
  
  def declare_is_covered(subjects) do
    subjects |> map(&"(declare-const #{is_covered(&1)} Bool)") |> join("\n")
  end
  
  
  # All the places in the resulting set must be occupied by some teacher, and only
  # one teacher
  
  # Place 1: a_1_1 \/ a_2_1 \/ ... \/ a_n_1
  # .....
  # Place k: a_1_k \/ a_2_k \/ ... \/ a_n_k

  # Place 1, teacher 1: a_1_1 => ¬a_2_1 /\ ¬a_3_1 /\ ...
  # Place 2, teacher 1: a_1_2 => ¬a_2_2 /\ ¬a_3_2 /\ ...
  # .....
  # Place k, teacher m: a_m_k => ¬a_1_k /\ ¬a_2_k /\ ...
  
  def all_places_occupied(teachers, k) do
    for pl <- 1..k do
      disjuncts = (for t <- teachers, do: teacher_in_place(t, pl)) |> join(" ")
      "(assert (or #{disjuncts}))"
    end |> join("\n")
  end
  
  def one_teacher_per_place(teachers, k) do
    for t <- teachers, pl <- 1..k do
      conjuncts = for t2 <- teachers, t2 != t do
        "(not #{teacher_in_place(t2, pl)})"
      end |> join(" ")
      "(assert (=> #{teacher_in_place(t, pl)} (and #{conjuncts})))"
    end |> join("\n")
  end
  

  # The i-th teacher has been selected if it occupies a place in the resulting set
  
  # Teacher 1: t_1 <=> a_1_1 \/ a_1_2 \/ ...
  # .....
  # Teacher n: t_n <=> a_n_1 \/ a_n_2 \/ ...
  
  def teacher_selected_definition(teachers, k) do
    for t <- teachers do
      disjuncts = (for pl <- 1..k, do: teacher_in_place(t, pl)) |> join(" ")
      "(assert (= #{selected_teacher(t)} (or #{disjuncts})))"
    end |> join("\n")
  end
  
  # A subject is covered if one of the teachers that can teach it has been selected
  # in the resulting set
  #
  # Assume that c_1 can be taught by teachers 2, 6 and 7
  #
  # Subject 1: c_1 <=> t_2 /\ t_6 /\ t_7
  # ....
  # Subject m: c_m <=> ...
  
  def subject_covered(subjects, teachers, coverage) do
    for s <- subjects do
      disjuncts = (for t <- teachers, s in coverage[t], do: selected_teacher(t)) |> join(" ")
      case disjuncts do
        "" -> "(assert (not #{is_covered(s)}))"
        _ -> "(assert (= #{is_covered(s)} (or #{disjuncts})))"
      end
    end |> join("\n")
  end
  
  # All subjects must be covered:
  #
  # c_1 /\ c_2 /\ ... /\ c_m
  
  def all_subjects_covered(subjects) do
    conjuncts = (for s <- subjects, do: is_covered(s)) |> join(" ")
    "(assert (and #{conjuncts}))"
  end
  
  
  

  def cover(teachers, subjects, coverage, k) do
    declare_selected_teacher(teachers) <> "\n"
    <> declare_teacher_in_place(teachers, k) <> "\n"
    <> declare_is_covered(subjects) <> "\n"
    <> all_places_occupied(teachers, k) <> "\n"
    <> teacher_selected_definition(teachers, k) <> "\n"
    <> one_teacher_per_place(teachers, k) <> "\n"
    <> subject_covered(subjects, teachers, coverage) <> "\n"
    <> all_subjects_covered(subjects) <> "\n"
    <> "(check-sat)\n"
    <> "(get-model)"
  end
end

teachers = ["alice", "mary", "john", "robert"]
subjects = ["algebra", "calculus", "history", "music", "arts", "ICT"]
coverage = %{
  "alice" => ["algebra", "calculus", "ICT"],
  "mary" => ["ICT"],
  "john" => ["arts", "history", "ICT"],
  "robert" => ["arts", "history"]
}

Subjects.cover(teachers, subjects, coverage, 3) |> IO.puts

