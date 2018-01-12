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

    Ns = py.array([1,2,4,6,12,16,24])
    info = {}
    for filename in filenames:
        info[filename] = {}
        for N in Ns:
            info_temp, data = PC.read_data(r"DATA/"+filename+"_Nt%02d.dat"%N)
            info[filename][N] = info_temp

        info[filename]["Wall_time"] = py.array([info[filename][N]["Wall_time"] for N in Ns])
        info[filename]["Sp"] = info[filename]["Wall_time"][0]/info[filename]["Wall_time"]
        info[filename]["T1"] = info[filename]["Wall_time"][0]
        info[filename]["k"] = info[filename][1]["k"]
        info[filename]["N"] = info[filename][1]["N"]
        info[filename]["lat"] = info[filename]["k"]*info[filename]["N"]**2/info[filename]["Wall_time"]


    # Plotting speed up effeciency
    fit_lw = 0.5
    fig, ax = py.subplots(1, 1, figsize=PC.fig_size)
    for filename in info:
        optf = curve_fit(lambda Ns, f, T0: TP(info[filename]["T1"],Ns,f,T0),Ns,info[filename]["Wall_time"],[0.5,0])[0]
        plt = ax.plot(Ns, info[filename]["Sp"], '.-',label=filename +" T0=%1.1e f=%1.2f"%(optf[1],optf[0]))
        ax.plot(Ns, info[filename]["T1"]/TP(info[filename]["T1"],Ns,*optf), '--',color=plt[0].get_color(),lw=fit_lw)
    ax.plot(py.arange(0,Ns[-1]), py.arange(0,Ns[-1]), 'k-',label="Ideal")
    ax.plot([py.nan], [py.nan], '--k', label="Fit",lw=fit_lw)
    ax.grid('on')
    ax.set_xlabel("N threads")
    ax.set_ylabel("S(P)")
    ax.legend()
    py.tight_layout()
    fig.savefig(PC.fig_path+fig_file+"_SP.png")

    fig, ax = py.subplots(1, 1, figsize=PC.fig_size)
    for filename in info:
        ax.plot(Ns, info[filename]["lat"], '.-', label=filename + " T1=%1.1e" % info[filename]["T1"])
    ax.grid('on')
    ax.set_xlabel("N threads")
    ax.set_ylabel(r"$N²k/t_{wall}$ ($\propto FLOPS/s$)")
    ax.legend()
    py.tight_layout()
    fig.savefig(PC.fig_path+fig_file+"_lat.png")

    py.show()

def TP(T1,Ns,f,T0):
    TP = f/Ns*T1+(1-f)*T1 +T0
    return(TP)

if __name__ == "__main__":
    main()