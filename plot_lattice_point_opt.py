import pylab as py
import plot_convergence as PC

def main():
    Ns = py.array([20,40,80,160,320])
    info_JAC = []
    for N in Ns:
        filename = "DATA/jaccon_N%04d.dat" % N
        info, data = PC.read_data(filename,9)
        info["lat"] = info["N"]**2*info["k"] / info["Wall_time"]
        info_JAC.append(info)
    info_GS = []
    for N in Ns:
        filename = "DATA/gscon_N%04d.dat" % N
        info, data = PC.read_data(filename,9)
        info["lat"] = info["N"]**2*info["k"] / info["Wall_time"]
        info_GS.append(info)


    fig,ax = py.subplots(1,1,figsize=PC.fig_size)
    ax.plot(Ns,PC.get_data(info_JAC, "lat"),'.-',label="Jacobi")
    ax.plot(Ns,PC.get_data(info_GS, "lat"),'.-',label="Gauss-Seidel")
    ax.grid('on')
    ax.set_xlabel("N (grid size)")
    ax.set_ylabel(r"$N^2k/t_{wall}$ ($\propto FLOPS/s$)")
    ax.legend()
    py.tight_layout()
    fig.savefig(PC.fig_path+"lat.png")
    py.show()


if __name__ == "__main__":
    main()