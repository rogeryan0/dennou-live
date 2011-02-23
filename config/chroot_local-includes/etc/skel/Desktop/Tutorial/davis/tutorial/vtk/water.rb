require "numru/gphys"
require "vtk"
include Vtk
include NumRu


p "open"
fname = "water.nc"
vname = "Cloud_water_content"
  
gphys = GPhys::IO.open(fname,vname)
shape = gphys.shape
val = gphys.val


x = gphys.coord(0).val
y = gphys.coord(1).val
yfact = (x.max-x.min)/(y.max-y.min)
z = gphys.coord(2).val
zfact = (x.max-x.min)/(z.max-z.min)
x = x.to_va
y = y.to_va
z = z.to_va
f = val.reshape!(shape[0]*shape[1]*shape[2]).to_va

data = RectilinearGrid.new
data.SetDimensions(*shape)
data.SetXCoordinates(x)
data.SetYCoordinates(y)
data.SetZCoordinates(z)
data.GetPointData.SetScalars(f)




surface = ContourFilter.new
surface.SetInput(data)
surface.SetValue(0,0.0007)

mapper = PolyDataMapper.new
mapper.SetInput(surface.GetOutput)
mapper.SetScalarModeToUsePointFieldData

actor = Actor.new
actor.SetMapper(mapper)
actor.GetProperty.SetOpacity(0.5)
actor.SetScale(1,yfact,zfact)

outline = RectilinearGridOutlineFilter.new
outline.SetInput(data)
mapper_ol = PolyDataMapper.new
mapper_ol.SetInput(outline.GetOutput)
actor_ol = Actor.new
actor_ol.SetMapper(mapper_ol)
actor_ol.SetScale(1,yfact,zfact)


#axes
bounds = data.GetBounds
length = data.GetLength
axes = Axes.new
axes.SetOrigin(bounds[0],bounds[2],bounds[4])
axes.SetScaleFactor(length*0.8)
tube_ax = TubeFilter.new
tube_ax.SetInput(axes.GetOutput)
tube_ax.SetRadius(length/50)
tube_ax.SetNumberOfSides(6)
mapper_ax = PolyDataMapper.new
mapper_ax.SetInput(tube_ax.GetOutput)
actor_ax = Actor.new
actor_ax.SetMapper(mapper_ax)


# graphics stuff
renderer = Renderer.new
renWin = RenderWindow.new
renWin.AddRenderer(renderer)
iren = RenderWindowInteractor.new
iren.SetRenderWindow(renWin)

# read data  //set up renderer
renderer.AddActor(actor_ax)
renderer.AddActor(actor_ol)
renderer.AddActor(actor)
renderer.SetBackground(1,1,1)
renWin.SetSize(500,500)
renderer.SetBackground(0.1, 0.2, 0.4)

# interact with data
iren.Initialize

renWin.Render
iren.Start

