module Scale where

data Target = Slot Int | Day Int | Cell {day::Int, slot::Int} | SpecLesson Int
data Rule = Rule {target::Target, severity::Int}
