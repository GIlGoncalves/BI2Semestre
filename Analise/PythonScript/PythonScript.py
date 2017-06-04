import csv
import sys
csv.field_size_limit(sys.maxsize)

from Generos import GENERO


def parser(data):
    res = 0
    split = data[9].split("|")
    for words in split:
        if words in GENERO:
            row[GENERO[words]]=1
        else: print(words)
    return res


in_file = open("movie_metadataVirgula.csv", "rt")

out_file = open("outMovieFinal.csv", "wt")


try:
    reader = csv.reader(in_file)
    writer = csv.writer(out_file)
    for row in reader:
        parser(row)
        writer.writerow(row)
finally:
    in_file.close()
    out_file.close()