import pylab as py
import plot_convergence as PC

def main():
    filename = "DATA/DATA_N0100_Nt04.dat"
    # Load data
    print("Reading data file: %s" % filename)
    info, data = PC.read_data(filename,9)
    for field in info:
        print(field + '= ', info[field])

    N = info["N"]
    dx = 2/(N-1)
    x = py.arange(N)*dx-1

    fig,ax = py.subplots(1,1,figsize=(8,6))
    pcol = ax.pcolor(x,x,data)
    fig.colorbar(pcol,ax=ax)
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.set_title("Solver type: "+ info["solver_type"])
    py.show()

if __name__ == "__main__":
    main()