require "vtk/Gtk"
require "numru/gphys"


###
# Now actually create the GUI
Gtk.init
Gtk::GL.init
root = Gtk::Window.new(Gtk::Window::TOPLEVEL)
top = Gtk::VBox.new
root.add(top)
@renWin = GtkGLExtVTKRenderWindow.new
top.pack_start(@renWin)


# Start by loading some data.
if ARGV.length==1
  gphys = NumRu::GPhys::IO.open_gturl(ARGV[0])
elsif ARGV.length==2
  fname,vname = ARGV[0..1]
  gphys = NumRu::GPhys::IO.open(fname,vname)
else
  raise "Usage: ruby #$0 filename varname"
end
gphys.rank<3 && raise("data must have at least 3 dimensions")
ind = Array.new(gphys.rank,0)
ind[0] = true
ind[1] = true
ind[2] = true
gphys = gphys[*ind]
shape = gphys.shape
min = gphys.min.val
max = gphys.max.val
data = (gphys.val-min)/(max-min)
NArrayMiss===data && data = data.get_array

x = gphys.coord(0).val
x = (x-x.min)/(x.max-x.min)
y = gphys.coord(1).val
y = (y-y.min)/(y.max-y.min)
z = gphys.coord(2).val
z = (z-z.min)/(z.max-z.min)

grid = Vtk::RectilinearGrid.new
grid.SetDimensions(*shape)
grid.SetXCoordinates(x.to_va)
grid.SetYCoordinates(y.to_va)
grid.SetZCoordinates(z.to_va)
grid.GetPointData.SetScalars(data.reshape!(shape[0]*shape[1]*shape[2]).to_va)

@xMin = x.min
@xMax = x.max
@yMin = y.min
@yMax = y.max
@zMin = z.min
@zMax = z.max




# An outline is shown for context.
outline = Vtk::OutlineFilter.new
outline.SetInput(grid)

outlineMapper = Vtk::PolyDataMapper.new
outlineMapper.SetInput(outline.GetOutput())

outlineActor = Vtk::Actor.new
outlineActor.SetMapper(outlineMapper)

# The 3 image plane widgets are used to probe the dataset.
@planeWidgetX = Vtk::Plane.new
@planeWidgetX.SetOrigin((@xMax+@xMin)/2,(@yMax+@yMin)/2,(@zMax+@zMin)/2)
@planeWidgetX.SetNormal(1,0,0)
cutX = Vtk::Cutter.new
cutX.SetInput(grid)
cutX.SetCutFunction(@planeWidgetX)
mapX = Vtk::PolyDataMapper.new
mapX.SetInput(cutX.GetOutput)
mapX.SetScalarRange(0,1)
actX = Vtk::Actor.new
actX.SetMapper(mapX)

@planeWidgetY = Vtk::Plane.new
@planeWidgetY.SetOrigin((@xMax+@xMin)/2,(@yMax+@yMin)/2,(@zMax+@zMin)/2)
@planeWidgetY.SetNormal(0,1,0)
cutY = Vtk::Cutter.new
cutY.SetInput(grid)
cutY.SetCutFunction(@planeWidgetY)
mapY = Vtk::PolyDataMapper.new
mapY.SetInput(cutY.GetOutput)
mapY.SetScalarRange(0,1)
actY = Vtk::Actor.new
actY.SetMapper(mapY)

@planeWidgetZ = Vtk::Plane.new
@planeWidgetZ.SetOrigin((@xMax+@xMin)/2,(@yMax+@yMin)/2,(@zMax+@zMin)/2)
@planeWidgetZ.SetNormal(0,0,1)
cutZ = Vtk::Cutter.new
cutZ.SetInput(grid)
cutZ.SetCutFunction(@planeWidgetZ)
mapZ = Vtk::PolyDataMapper.new
mapZ.SetInput(cutZ.GetOutput)
mapZ.SetScalarRange(0,1)
actZ = Vtk::Actor.new
actZ.SetMapper(mapZ)



# Create the RenderWindow and Renderer
@ren = Vtk::Renderer.new
@renWin.GetRenderWindow.AddRenderer(@ren)

# Add the outline actor to the renderer, set the background color and size
@ren.AddActor(outlineActor)
@ren.AddActor(actX)
@ren.AddActor(actY)
@ren.AddActor(actZ)
@renWin.set_size_request(500, 500)
@ren.SetBackground(0.1, 0.1, 0.2)

@current_Axis = "Z"

# Create the GUI
# We first create the supporting functions (callbacks) for the GUI
#
# Align the camera so that it faces the desired widget
def AlignCamera
  cx = (@xMax+@xMin)/2
  cy = (@yMax+@yMin)/2
  cz = (@zMax+@zMin)/2
  vx, vy, vz = 0, 0, 0
  nx, ny, nz = 0, 0, 0
  case @current_Axis
  when "X"
    vz = 1
    nx = (@xMax-@xMin)/2
    cx = @xMin + (@xMax-@xMin)*@x_adjust.value
  when "Y"
    vz = 1
    ny = (@yMax-@yMin)/2
    cy = @yMin + (@yMax-@yMin)*@y_adjust.value
  when "Z"
    vy = 1
    nz = (@zMax-@zMin)/2
    cz = @zMin + (@zMax-@zMin)*@z_adjust.value
  end
 
  px = cx+nx*5.2
  py = cy+ny*5.2
  pz = cz+nz*5.3

  camera = @ren.GetActiveCamera()
  camera.SetViewUp(vx, vy, vz)
  camera.SetFocalPoint(cx, cy, cz)
  camera.SetPosition(px, py, pz)
  camera.OrthogonalizeViewUp()
  @ren.ResetCameraClippingRange()
  @renWin.Render()
end
 
# Capture the display and place in a PNG
def captureImage()
  w2i = Vtk::WindowToImageFilter.new
  writer = Vtk::PNGWriter.new
  w2i.SetInput(@renWin.GetRenderWindow)
  w2i.Update()
  writer.SetInput(w2i.GetOutput())
  writer.SetFileName("image.png")
  @renWin.Render()
  writer.Write()
end
 

# Align the widget back into orthonormal position,
# set the slider to reflect the widget's position,
# call AlignCamera to set the camera facing the widget
def alignXaxis()
  @current_Axis = "X"
  AlignCamera()
end

def alignYaxis()
  @current_Axis = "Y"
  AlignCamera()
end
 
def alignZaxis()
  @current_Axis = "Z"
  AlignCamera()
end


        
def SetSlice(adj,i)
  case i
  when 0
    @planeWidgetX.SetOrigin(@xMin+(@xMax-@xMin)*adj.value,0.5,0.5)
  when 1
    @planeWidgetY.SetOrigin(0.5,@yMin+(@yMax-@yMin)*adj.value,0.5)
  when 2
    @planeWidgetZ.SetOrigin(0.5,0.5,@zMin+(@zMax-@zMin)*adj.value)
  end
  @ren.ResetCameraClippingRange()
  @renWin.Render()
end




# Buttons
ctrl_buttons = Gtk::HBox.new

quit_button = Gtk::Button.new("Quit")
quit_button.signal_connect("clicked"){ Gtk.main_quit }
capture_button = Gtk::Button.new("PNG")
capture_button.signal_connect("clicked"){ captureImage }
x_button = Gtk::Button.new("x")
x_button.signal_connect("clicked"){ alignXaxis }
y_button = Gtk::Button.new("y")
y_button.signal_connect("clicked"){ alignYaxis }
z_button = Gtk::Button.new("z")
z_button.signal_connect("clicked"){ alignZaxis }
ctrl_buttons.pack_start(quit_button)
ctrl_buttons.pack_start(capture_button)
ctrl_buttons.pack_start(x_button)
ctrl_buttons.pack_start(y_button)
ctrl_buttons.pack_start(z_button)
top.pack_start(ctrl_buttons)



# Add a slice scale to browse the current slice stack

x_slice = Gtk::HScale.new(0,1,0.01)
y_slice = Gtk::HScale.new(0,1,0.01)
z_slice = Gtk::HScale.new(0,1,0.01)
x_slice.value = 0.5
y_slice.value = 0.5
z_slice.value = 0.5
@x_adjust = x_slice.adjustment
@y_adjust = y_slice.adjustment
@z_adjust = z_slice.adjustment
@x_adjust.signal_connect("value-changed"){|w| SetSlice(w,0) }
@y_adjust.signal_connect("value-changed"){|w| SetSlice(w,1) }
@z_adjust.signal_connect("value-changed"){|w| SetSlice(w,2) }
top.pack_start(x_slice)
top.pack_start(y_slice)
top.pack_start(z_slice)

# Done with the GUI. 
###


# Create an initial interesting view
cam1 = @ren.GetActiveCamera()
cam1.Elevation(-60)
cam1.SetViewUp(0, 0, 1)
cam1.Azimuth(30)
@ren.ResetCameraClippingRange()



# Start Tkinter event loop
root.show_all
Gtk.main