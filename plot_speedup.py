import pylab as py
import sys
import plot_convergence as PC


def main():
    # Reading input arguments
    filenames = []
    if len(sys.argv) > 1:
        for filename in sys.argv[1:]:
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

    fig, ax = py.subplots(1, 1, figsize=PC.fig_size)
    for filename in info:
        ax.plot(Ns, info[filename]["Sp"], '.-',label=filename +" T1=%1.1e"%info[filename]["T1"])
    ax.plot(py.arange(0,Ns[-1]), py.arange(0,Ns[-1]), 'k-',label="Ideal")
    ax.grid('on')
    ax.set_xlabel("N threads")
    ax.set_ylabel("S(P)")
    ax.legend()
    py.tight_layout()
    py.show()

if __name__ == "__main__":
    main()