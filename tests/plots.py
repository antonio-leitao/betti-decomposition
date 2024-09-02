import matplotlib.pyplot as plt
import pandas as pd

fig, ax = plt.subplots(figsize=(4.8, 2.6), dpi=200)
colors = ["#0283F8", "#7D49CB", "#FF0059"]

for i in range(1, 4):
    df = pd.read_csv(f"data/sequential_opt{i}.csv")
    ax.plot(df["n_simplicies"], df["time"], color=colors[i - 1], marker=".", label=i)
ax.spines[["right", "top"]].set_visible(False)
ax.set_xlabel("Number of Simplicies")
ax.set_ylabel("Execution Time (s)")
plt.legend(title="Optimization", frameon=False, loc="lower right")
plt.yscale("log")
plt.xscale("log")
plt.tight_layout()
plt.savefig("assets/sequential.png")
plt.show()
plt.close()

fig, ax = plt.subplots(figsize=(4.8, 2.6), dpi=200)
colors = ["#0283F8", "#7D49CB", "#FF0059"]

for i in range(1, 4):
    df = pd.read_csv(f"data/parallel_opt{i}.csv")
    ax.plot(df["n_simplicies"], df["time"], color=colors[i - 1], marker=".", label=i)
ax.spines[["right", "top"]].set_visible(False)
ax.set_xlabel("Number of Simplicies")
ax.set_ylabel("Execution Time (s)")
plt.legend(title="Optimization", frameon=False, loc="lower right")
plt.yscale("log")
plt.xscale("log")
plt.tight_layout()
plt.savefig("assets/parallel.png")
plt.show()
plt.close()

fig, ax = plt.subplots(figsize=(4.8, 2.6), dpi=200)
colors = ["#0283F8", "#7D49CB", "#FF0059"]

df = pd.read_csv(f"data/sequential_opt1.csv")
ax.plot(df["n_simplicies"], df["time"], color=colors[0], marker=".", label="Sequential")
df = pd.read_csv(f"data/parallel_opt1.csv")
ax.plot(df["n_simplicies"], df["time"], color=colors[2], marker=".", label="Parallel")
ax.spines[["right", "top"]].set_visible(False)
ax.set_xlabel("Number of Simplicies")
ax.set_ylabel("Execution Time (s)")
plt.legend(frameon=False, loc="lower right")
plt.yscale("log")
plt.xscale("log")
plt.tight_layout()
plt.savefig("assets/parallel_sequential_benchmark.png")
plt.show()
plt.close()
