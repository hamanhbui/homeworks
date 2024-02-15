import numpy as np
from examples import bumps
from matplotlib import pyplot as plt



fun = bumps
u, v = np.meshgrid(np.linspace(-2, 2, 100), np.linspace(-2, 2, 100))
f = u**2 +  v**2

plt.figure('surf')
fig, ax = plt.subplots(subplot_kw={"projection": "3d"})
# Plot the surface.
surf = ax.plot_surface(u, v, f, #cmap=cm.coolwarm,
                       linewidth=0, antialiased=False)
plt.show()