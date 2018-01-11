import pylab as py
import plot_convergence as PC

def main():
    Ns = py.array([20,40,80,160,320])
    info_JAC = []
    for N in Ns:
        filename = "DATA/jaccon_N%04d.dat" % N
        info, data = PC.read_data(filename)
        info["lat"] = info["N"]*info["k"] / info["Wall_time"]
        info_JAC.append(info)
    info_GS = []
    for N in Ns:
        filename = "DATA/gscon_N%04d.dat" % N
        info, data = PC.read_data(filename)
        info["lat"] = info["N"]*info["k"] / info["Wall_time"]
        info_GS.append(info)


    fig,ax = py.subplots(1,1,figsize=(12,7))
    ax.loglog(Ns,get_data(info_JAC, "lat"),'.-',label="Jacobi")
    ax.loglog(Ns,get_data(info_GS, "lat"),'.-',label="Gauss-Seidel")
    ax.grid('on')
    ax.set_xlabel("N (grid size)")
    ax.set_ylabel("lattice site updates per second")
    ax.legend()
    py.tight_layout()
    py.show()

def get_data(infos, field):
    out = [data[field] for data in infos]
    return(py.array(out))

if __name__ == "__main__":
    main()