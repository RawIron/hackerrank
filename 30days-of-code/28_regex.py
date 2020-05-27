#!/bin/python3

import re


def read_input():
    total = int(input())

    items = []
    for _ in range(total):
        items.append(input().split())
    return items


def has_gmail(emails):
    ''' filter is_gmail emails '''
    gmail_addr = []
    gmail_pattern = re.compile(r'[a-z]+(\.[a-z]+)?@gmail\.com$')

    for (first_name, email_addr) in emails:
        is_gmail = gmail_pattern.match(email_addr)
        if is_gmail:
            gmail_addr.append(first_name)

    return gmail_addr


if __name__ == '__main__':
    # read | filter | show
    emails_in = read_input()

    for name in sorted(has_gmail(emails_in)):
        print(name)
