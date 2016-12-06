"""programm for calculating formulas in csv"""
import csv
import re
import sys

TASKS = {
    '0.4': '0.4-Minimal-input.csv',
    '0.5': '0.5-WithReferences-input.csv',
    '0.6': '0.6-UseMath-input.csv',
    '0.7': '0.7-CustomScript-input.csv',
    '0.8': 'plus0.1-FarReferences-input.csv'
}

TASK = '0.8'
CSV_TABLE = []

INPUT_FILE = sys.argv[1]
OUTPUT_FILE = sys.argv[2]
MODULE_FILE = ""
if len(sys.argv) > 3:
    MODULE_FILE = sys.argv[3]


def read_table(table):
    """read data from csv"""
    csvfile = open(INPUT_FILE, 'r')
    reader = csv.reader(csvfile)
    for row in reader:
        table.append(row)
    return reader.dialect


def str_to_val(str_val):
    """converts cell value to number or float if possible"""
    try:
        return int(str_val)
    except ValueError:
        try:
            return float(str_val)
        except ValueError:
            return str(str_val)


def replace_references(table):
    """replaces references in cells to real values from table"""
    for i, _ in enumerate(table):
        for j, table_cell in enumerate(table[i]):
            if table_cell[0] == "=":
                tokens = re.split(r'(\w\d+)', table_cell[1:])
                form_cell = "="
                for token in tokens:
                    if len(token) > 0 and token[0] >= 'A' and token[0] <= 'Z':
                        pos_x = ord(token[0]) - ord('A')
                        pos_y = int(token[1:]) - 1
                        expr = "str_to_val(CSV_TABLE[" \
                            + str(pos_y) + "][" \
                            + str(pos_x) + "])"
                        form_cell += expr
                    else:
                        form_cell += token
                table[i][j] = form_cell


def execute_cells(table):
    """executes all expressions in cells"""
    if MODULE_FILE != "":
        with open(MODULE_FILE) as script_file:
            exec("from math import *\n" + script_file.read())

    for i, _ in enumerate(table):
        for j, cell in enumerate(table[i]):
            if cell[0] == "=":
                table[i][j] = eval(cell[1:], globals(), locals())


def save_table(table, dialect):
    """saves table to csv file"""
    result_file = open(OUTPUT_FILE, 'w')
    writer = csv.writer(result_file, dialect)
    for row in table:
        writer.writerow(row)
        result_file.write("\n")
    result_file.close()


DIALECT = read_table(CSV_TABLE)
replace_references(CSV_TABLE)
execute_cells(CSV_TABLE)
save_table(CSV_TABLE, DIALECT)
