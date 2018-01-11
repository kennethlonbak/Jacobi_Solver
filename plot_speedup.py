import pylab as py
import plot_convergence as PC

def main():
    Ns = py.array([1,2,4,8,16,32])
    info_JAC = []
    for N in Ns:
        filename = "DATA/pal1_N0400_Nt%02d.dat" % N
        info, data = PC.read_data(filename)
        info["lat"] = info["N"]*info["k"] / info["Wall_time"]
        info_JAC.append(info)

    fig, ax = py.subplots(1, 1, figsize=(12, 7))
    wtime = PC.get_data(info_JAC,"Wall_time")
    speed_up = wtime[0]/wtime[1:]
    eff = speed_up/Ns[1:]
    ax.plot(Ns[1:], speed_up, '.-')
    ax.plot(Ns, Ns, 'k.-')
    ax.grid('on')
    ax.set_xlabel("N threads")
    ax.set_ylabel("S(P)")
    ax.legend()
    py.tight_layout()
    py.show()

if __name__ == "__main__":
    main()