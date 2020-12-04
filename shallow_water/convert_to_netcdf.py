from iris import save
from iris.coords import DimCoord
from iris.cube import Cube, CubeList
import numpy as np
from os import listdir
from glob import glob


def create_cube(data, name):
    ntime, nx, ny = data.shape
    print(data.shape)
    xs = np.linspace(0, float(nx), nx, dtype=np.float32)
    ys = np.linspace(0, float(ny), ny, dtype=np.float32)
    time = DimCoord(np.arange(ntime, dtype=np.float32), standard_name='time', var_name='time',\
        units=1)
    x = DimCoord(xs, standard_name='latitude', long_name='latitude', var_name='x')
    y = DimCoord(ys, standard_name='longitude', long_name='longitude', var_name='y')

    return Cube(data, dim_coords_and_dims=[(time,0), (x, 1), (y, 2)], long_name=name, var_name=name)


h_data = []
for filename in sorted(glob("./output/h*.txt"), key=lambda name: int(name[11:-4])):
    h_data.append(np.loadtxt(filename))

print('Converting to NetCDF...')
h_cube = create_cube(np.array(h_data), 'height')
save(CubeList([h_cube]), 'output.nc', unlimited_dimensions=['time'])
print('Finished')
