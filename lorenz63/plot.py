import matplotlib.pyplot as plt
import numpy as np

# Load model data
trajectory = np.loadtxt("output.txt")

# Create subplot figure window
fig, (ax1, ax2, ax3) = plt.subplots(3, sharex=True)

# Plot all three variables
ax1.plot(trajectory[:,0])
ax2.plot(trajectory[:,1])
ax3.plot(trajectory[:,2])

# Label plot
ax1.set_ylabel("x")
ax2.set_ylabel("y")
ax3.set_ylabel("z")
ax3.set_xlabel("Time step")

plt.show()

