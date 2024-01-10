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