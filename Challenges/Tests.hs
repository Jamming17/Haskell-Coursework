import Challenges
import Parsing

-- All tests should output True if they are successfully computed

-- Tests for Challenge 1: isPuzzleComplete
testC1 :: (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
testC1 = (t1, t2, t3, t3andahalf, t4, t5, t6, t7) where
    -- Test cases provided by the coursework description
    t1 = not (isPuzzleComplete [ [ Wire [North,West] , Wire [North,South] , Source [North] ], [ Wire [North,West], Wire [East,West], Wire [North,East] ], [ Sink [West] , Wire [North,South] , Wire [North,West] ] ])
    t2 = isPuzzleComplete [ [ Wire [South,East] , Wire [East,West] , Source [West] ], [ Wire [North,East], Wire [East,West], Wire [South,West] ], [ Sink [East] , Wire [East,West] , Wire [North,West] ] ]
    -- Two directly connected sinks and sources
    t3 = (isPuzzleComplete [[Sink [East], Source [West]], [Wire [], Sink [South]], [Wire [], Source [North]]]) == True
    -- Connected puzzle with disconnected sinks and sources
    t3andahalf = (isPuzzleComplete [[Sink [South], Wire [South]], [Wire [North], Source [North]]]) == False
    -- Two sinks leading to the same source
    t4 = (isPuzzleComplete [[Wire [], Source [South], Wire []], [Wire [South, East], Wire [North, East, West], Wire [South, West]], [Sink [North], Wire [], Sink [North]]])
    -- Puzzles with no source / no sink / no sources or sinks
    t5 = (isPuzzleComplete [[Wire [South, East], Wire [West]], [Sink [North], Wire []]]) == False
    t6 = (isPuzzleComplete [[Wire [South, East], Wire [West]], [Source [North], Wire []]]) == False
    t7 = (isPuzzleComplete [[Wire [South, East], Wire [West]], [Sink [North], Wire []]]) == False
    -- A larger 4x4 connected puzzle with sources and sinks
    t8 = (isPuzzleComplete [[Wire [East, South], Wire [South, West], Wire [], Wire []], [Source [North], Wire [North, East], Wire [South, West], Wire []], [Wire [East, South], Sink [West], Wire [North, East, South], Sink [West]], [Wire [North, East], Source [West], Wire [North, East], Wire [West]]]) == False

    -- Tests for Challenge 2: solveCircuit
testC2 :: (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
testC2 = (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) where
    -- Test case provided by the coursework description
    t1 = (solveCircuit [[Wire [North, West], Wire [North, South], Source [North]], [Wire [North, West], Wire [East, West], Wire [North, East] ], [Sink [West], Wire [North, South], Wire [North, West]]]) == Just [[R180, R90, R270], [R90, R0, R180], [R180, R90, R0]]
    -- Solveable puzzle with just sources and sinks
    t2 = (solveCircuit [[Source [North], Sink [West]], [Wire [], Wire []], [Sink [North], Source [East]]]) == Just [[R90, R0], [R0, R0], [R90, R180]]
    -- Connectable puzzle with no sources and sinks
    t3 = (solveCircuit [[Wire [North, East], Wire [North]], [Wire [North, West], Wire [West]]]) == Nothing
    -- Unsolveable puzzle with sources and sinks
    t4 = (solveCircuit [[Sink [North], Wire []], [Wire [], Source [East]]]) == Nothing
    -- A single-line solveable 3x3 puzzle
    t5 = (solveCircuit [[Source [North], Wire [North, East], Wire []], [Wire [], Wire [East, West], Wire []], [Wire [], Wire [North, East], Sink [East]]]) == Just [[R90, R180, R0], [R0, R90, R0], [R0, R0, R180]]
    -- A puzzle with multiple loops and paths between the source and sink
    t6 = (solveCircuit [[Source [North], Wire [North, East], Wire [North, East]], [Wire [North, West], Wire [North, East, South, West], Wire [North, East, West]], [Wire [], Sink [East, South], Wire [North, East]]]) == Just [[R180, R90, R180], [R90, R0, R270], [R0, R270, R270]]
    -- A puzzle split into two lines
    t7 = (solveCircuit [[Wire [North], Wire [], Source [East]], [Wire [North, South], Wire [], Wire [East, West]], [Wire [East], Wire [], Sink [East]]]) == Just [[R180, R0, R90], [R0, R0, R90], [R270, R0, R270]]
    -- A puzzle with three sources and three sinks
    t8 = (solveCircuit [[Sink [West], Wire [], Wire []], [Source [North], Wire [], Source [North]], [Wire [], Wire [], Sink [East]], [Sink [North], Source [South], Wire []]]) == Just [[R270, R0, R0], [R0, R0, R180], [R0, R0, R270], [R90, R90, R0]]
    -- A fully-wired puzzle
    t9 = (solveCircuit [[Source [East, South], Wire [East, North, West], Wire [North, East]], [Wire [North, South, West], Wire [North, South, East, West], Wire [East, North, West]], [Wire [North, East], Wire [South, West, North], Sink [South, East]]]) == Just [[R0, R180, R180], [R180, R0, R270], [R0, R90, R180]]
    -- A fully-wired puzzle composed of just sinks and paths
    t10 = (solveCircuit [[Source [East, South], Sink [East, North, West], Source [North, East]], [Sink [North, South, West], Source [North, South, East, West], Sink [East, North, West]], [Source [North, East], Sink [South, West, North], Source [South, East]]]) == Just [[R0, R180, R180], [R180, R0, R270], [R0, R90, R180]]