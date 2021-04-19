_input_table = """method, machine, opt., speed-up, policy time [s], strategy time [s]
add/sub,Core 2 Duo,no,1.24, 51.67, 64.05
add/sub,Core 2 Duo,yes,4.13, 5.14, 21.27
max/min,Core 2 Duo,yes,1.76, 12.03, 21.17
Christoph’s add/sub,Core 2 Duo,yes,1.02, 44.29, 45.23
add/sub,i7,yes,3.69,3.41,12.59
max/min,i7,yes,2.31,5.44 ,12.58
Christoph’s add/sub,i7,yes,0.78,41.01 ,32.1
add/sub,Xeon,yes,4.04,2.64,10.66
max/min,Xeon,yes,1.55,17.81,27.69
Christoph’s add/sub,Xeon,yes,1.5,18.52,27.86"""

def main():
    res = "<table>"
    for i, l in enumerate(_input_table.split("\n")):
        if i == 0:
            cell = "<th>", "</th>"
        else:
            cell = "<td>", "</td>"
        res += "<tr>"
        res += "".join([f"{cell[0]}{li}{cell[1]}" for li in l.split(",")])
        res += "</tr>\n"
    res += "</table>"
    print(res)

if __name__ == "__main__":
    main()