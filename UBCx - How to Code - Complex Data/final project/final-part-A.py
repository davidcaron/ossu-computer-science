"""
Edx course
UBCx: HtC2x
How to Code: Complex Data

This is a python version of the final problem. Just to compare with the racket version.
"""

from collections import namedtuple, Counter

Ta = namedtuple("Ta", "name max avail")
Assignment = namedtuple("Assignment", "ta slot")


def schedule_tas(tas, slots, rsf=None):
    """
    return schedule or False if it's impossible
    :param tas: list of Ta
    :param slots: list of int
    :param rsf: "result so far" accumulator for the assignments
    :return:
    """
    if not len(slots):
        return rsf
    if rsf is None:
        rsf = []
    for a in make_assignments(tas, slots[0], rsf):
        value = schedule_tas(tas, slots[1:], [a] + rsf)
        if value:
            return value


def make_assignments(tas, slot, assignments):
    """
    return all possible assignments, considering tas's max availability and current assignments
    :param tas: list of Ta
    :param slot: slot to fill
    :param assignments: current assignments
    :type assignments: list of Assignment
    """
    assignments_by_name = Counter([a.ta.name for a in assignments])

    def ta_available(ta):
        return ta.name not in assignments_by_name or assignments_by_name[ta.name] < ta.max

    available_tas = filter(ta_available, tas)
    return [Assignment(ta, slot) for ta in available_tas if slot in ta.avail]


# === TESTS

def test_schedule_tas_simple():
    soba = Ta("Soba", 2, [1, 3])
    udon = Ta("Udon", 1, [3, 4])
    ramen = Ta("Ramen", 1, [2])
    tas = [soba, udon, ramen]
    slots = [1, 2, 3, 4]
    r = schedule_tas(tas, slots)
    expected = [Assignment(udon, 4),
                Assignment(soba, 3),
                Assignment(ramen, 2),
                Assignment(soba, 1)]
    assert r == expected


def test_schedule_tas_complicated():
    erika = Ta("Erika", 1, [1, 3, 7, 9])
    ryan = Ta("Ryan", 1, [1, 8, 10])
    reece = Ta("Reece", 1, [5, 6])
    gordon = Ta("Gordon", 2, [2, 3, 9])
    david = Ta("David", 2, [2, 8, 9])
    katie = Ta("Katie", 1, [4, 6])
    aashish = Ta("Aashish", 2, [1, 10])
    grant = Ta("Grant", 2, [1, 11])
    raeanne = Ta("Raeanne", 2, [1, 11, 12])
    erin = Ta("Erin", 1, [4])
    tas = [erika, ryan, reece, gordon, david, katie, aashish, grant, raeanne, erin]
    slots = list(range(1, 13))
    r = schedule_tas(tas, slots)
    expected = [Assignment(raeanne, 12),
                Assignment(grant, 11),
                Assignment(aashish, 10),
                Assignment(david, 9),
                Assignment(david, 8),
                Assignment(erika, 7),
                Assignment(katie, 6),
                Assignment(reece, 5),
                Assignment(erin, 4),
                Assignment(gordon, 3),
                Assignment(gordon, 2),
                Assignment(ryan, 1),
                ]
    assert r == expected
