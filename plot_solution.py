import pylab as py

def main():
    filename = "DATA_N0320.dat"
    # Load data
    info, data = read_data(filename)
    print("Reading data file: %s"%filename)
    N = info["N"]
    dx = 2/(N-1)
    x = py.arange(N)*dx-1
    x_grid, y_grid = py.meshgrid(x,x)
    ref_data = py.sin(py.pi*x_grid)*py.sin(py.pi*y_grid)

    fig,ax = py.subplots(1,1,figsize=(8,6))
    pcol = ax.pcolor(x,x,data-ref_data)
    fig.colorbar(pcol,ax=ax)
    py.show()

def read_data(filename):
    data = py.loadtxt(filename, skiprows=6)
    info = {}
    with open(filename,"r") as file:
        for i in range(5):
            line = file.readline().split("=")
            if isinteger(line[1]):
                info[line[0].strip()] = int(line[1])
            else:
                info[line[0].strip()] = float(line[1])

    return(info,data)

def isinteger(value):
  try:
    int(value)
    return True
  except:
    return False

if __name__ == "__main__":
    main()