import pylab as py
import sys
import plot_convergence as PC
from scipy.optimize import curve_fit

def main():
    # Reading input arguments
    filenames = []
    if len(sys.argv) > 1:
        fig_file = sys.argv[1]
        for filename in sys.argv[2:]:
            filenames.append(filename)
    else:
        filenames = ["pal1_k2000_N1000"]

    Ns = py.array([20,40,80,160,320])
    info = {}
    for filename in filenames:
        info[filename] = {}
        for N in Ns:
            info_temp, data = PC.read_data(r"DATA/"+filename+"_N%04d.dat"%N)
            info[filename][N] = info_temp

        info[filename]["Wall_time"] = py.array([info[filename][N]["Wall_time"] for N in Ns])
        info[filename]["Sp"] = info[filename]["Wall_time"][0]/info[filename]["Wall_time"]
        info[filename]["T1"] = info[filename]["Wall_time"][0]
        info[filename]["k"] = py.array([info[filename][N]["k"] for N in Ns])
        info[filename]["N"] = py.array([info[filename][N]["N"] for N in Ns])
        info[filename]["solver_type"] = info[filename][N]["solver_type"]
        info[filename]["lat"] = info[filename]["k"]*info[filename]["N"]**2/info[filename]["Wall_time"]


    # Plotting speed up effeciency
    PC.fig_size = (6,3)
    fig, ax = py.subplots(1, 1, figsize=PC.fig_size)
    for filename in info:
        ax.plot(Ns, info[filename]["lat"], '.-',label=info[filename]["solver_type"])
    ax.grid('on')
    ax.set_xlabel("N (grid)")
    ax.set_ylabel("LPUpS=$N^2k/t_{wall}$ ($\propto FLOPS/s$)")
    ax.legend()
    py.tight_layout()
    fig.savefig(PC.fig_path+fig_file+"_Lat.png")

    fig, ax = py.subplots(1, 1, figsize=PC.fig_size)
    for filename in info:
        ax.semilogy(Ns, info[filename]["Wall_time"], '.-', label=info[filename]["solver_type"])
    ax.grid('on')
    ax.set_xlabel("N (grid)")
    ax.set_ylabel("$t_{wall}$ (Wall time)")
    ax.legend()
    py.tight_layout()
    fig.savefig(PC.fig_path + fig_file + "_Wall.png")

    fig, ax = py.subplots(1, 1, figsize=PC.fig_size)
    for filename in info:
        ax.plot(Ns, info[filename]["k"], '.-', label=info[filename]["solver_type"])
    ax.grid('on')
    ax.set_xlabel("N (grid)")
    ax.set_ylabel("$k$")
    ax.legend()
    py.tight_layout()
    fig.savefig(PC.fig_path + fig_file + "_k.png")

    py.show()

def TP(T1,Ns,f,T0):
    TP = f/Ns*T1+(1-f)*T1 +T0
    return(TP)

if __name__ == "__main__":
    main()