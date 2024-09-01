import matplotlib.pyplot as plt
import pandas as pd

df = pd.read_csv("tests/results.csv")
fig, ax = plt.subplots(figsize=(4.8, 2.6), dpi=200)
ax.plot(df["n_simplicies"], df["time"],marker = ".")
ax.spines[["right", "top"]].set_visible(False)
ax.set_xlabel("Number of Simplicies")
ax.set_ylabel("Execution Time (s)")
plt.yscale("log")
plt.xscale("log")
plt.tight_layout()
plt.savefig("assets/benchmark.png")
plt.show()
