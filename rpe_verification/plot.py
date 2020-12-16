import matplotlib.pyplot as plt
import numpy as np

# Load data
data = np.loadtxt('output.txt')

plt.figure(figsize=(8,3))

# Plot relative difference between rpe half-precision values as a
# function of the value of the variable
plt.loglog(data[:,0], abs(data[:,1] - data[:,0])/data[:,0],
           linewidth=2, label="rpe")

# Plot relative difference between Fugaku half-precision values as a
# function of the value of the variable
plt.loglog(data[:,0], abs(data[:,2] - data[:,0])/data[:,0],
           linewidth=0.6, label="Fugaku")

# Label plot
plt.xlabel("Value of variable")
plt.ylabel("Relative error w.r.t. double-precision")
plt.legend()

plt.show()

