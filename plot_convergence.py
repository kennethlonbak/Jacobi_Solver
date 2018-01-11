import pylab as py

def main():
    Ns = py.array([20,40,80,160,320])
    infos = []
    max_error = []
    for name in ["jaccon", "gscon"]:
        infos.append([])
        max_error.append([])
        for N in Ns:
            filename = "DATA/%s_N%04d.dat"%(name,N)
            info, data = read_data(filename)
            dx = 2/(N-1)
            x = py.arange(N)*dx-1
            x_grid, y_grid = py.meshgrid(x,x)
            x_grid = x_grid.transpose()
            y_grid = y_grid.transpose()
            ref_data = py.sin(py.pi*x_grid)*py.sin(py.pi*y_grid)

            max_error[-1].append(py.amax(py.amax(abs(data-ref_data))))
            infos[-1].append(info)
    max_error = py.array(max_error)




    fig,ax = py.subplots(1,1,figsize=(6,4))
    # Fitting
    for ii, err in enumerate(max_error):
        a, b = py.polyfit(py.log(Ns), py.log(err), 1)
        infos[ii][0]["a"] = a
        ax.loglog(Ns,err,'.-',label=infos[ii][0]["solver_type"]+" (Fit $\propto N^{%2.2f}$)"%a)
        #ax.loglog(Ns,py.log(b)*Ns**a,label=r"Fit: Err $\propto N^{%2.2f}$"%a)
    ax.grid('on')
    ax.set_xlabel("N (grid size)")
    ax.set_ylabel("max|$u_{solver}-u_{ref}$| (max error)")
    ax.legend()
    py.tight_layout()
    py.show()

def read_data(filename, header_lines = 9):

    data = py.loadtxt(filename, skiprows=header_lines).transpose()
    info = {}
    with open(filename,"r") as file:
        for i in range(header_lines):
            line = file.readline().split("=")
            if isinteger(line[1]):
                info[line[0].strip()] = int(line[1])
            elif isfloat(line[1]):
                info[line[0].strip()] = float(line[1])
            else:
                info[line[0].strip()] = line[1].strip()

    return(info,data)

def isinteger(value):
  try:
    int(value)
    return True
  except:
    return False

def isfloat(value):
  try:
    float(value)
    return True
  except:
    return False

if __name__ == "__main__":
    main()