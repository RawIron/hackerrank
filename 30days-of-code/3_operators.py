#!/bin/python3

import math
import os
import random
import re
import sys

def solve(meal_cost, tip_percent, tax_percent):
    if (tip_percent < 0): tip_percent = 0
    if (tax_percent < 0): tax_percent = 0
    if (meal_cost < 0):   meal_cost = 0
    return meal_cost + meal_cost * (tip_percent/100.0) + meal_cost * (tax_percent/100.0)

if __name__ == '__main__':
    meal_cost = float(input())
    tip_percent = int(input())
    tax_percent = int(input())

    total = solve(meal_cost, tip_percent, tax_percent)
    print(round(total))
