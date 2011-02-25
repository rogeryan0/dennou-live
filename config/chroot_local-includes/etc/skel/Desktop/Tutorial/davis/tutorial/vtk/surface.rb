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
#data = (gphys.val-min)/(max-min)
data = gphys.val
NArrayMiss===data && data = data.get_array


x = gphys.coord(0).val
xwid = x.max-x.min
y = gphys.coord(1).val
ywid = y.max-y.min
z = gphys.coord(2).val
zwid = z.max-z.min

yfact = xwid/ywid
zfact = xwid/zwid


grid = Vtk::RectilinearGrid.new
grid.SetDimensions(*shape)
grid.SetXCoordinates(x.to_va)
grid.SetYCoordinates(y.to_va)
grid.SetZCoordinates(z.to_va)
grid.GetPointData.SetScalars(data.reshape!(shape[0]*shape[1]*shape[2]).to_va)




# An outline is shown for context.
outline = Vtk::OutlineFilter.new
outline.SetInput(grid)

outlineMapper = Vtk::PolyDataMapper.new
outlineMapper.SetInput(outline.GetOutput())

outlineActor = Vtk::Actor.new
outlineActor.SetMapper(outlineMapper)
outlineActor.SetScale(1,yfact,zfact)

# Isosurface
@surface = Vtk::ContourFilter.new
@surface.SetInput(grid)
@surface.SetValue(0,(max+min)/2)
#normal = Vtk::PolyDataNormals.new
#normal.SetInput(@surface.GetOutput)
#normal.SetFeatureAngle(45)
mapper = Vtk::PolyDataMapper.new
mapper.SetInput(@surface.GetOutput)
#mapper.SetInput(normal.GetOutput)
mapper.SetScalarModeToUsePointFieldData
actor = Vtk::Actor.new
actor.SetMapper(mapper)
actor.GetProperty.SetOpacity(0.5)
actor.SetScale(1,yfact,zfact)



# Create the RenderWindow and Renderer
@ren = Vtk::Renderer.new
@renWin.GetRenderWindow.AddRenderer(@ren)

# Add the outline actor to the renderer, set the background color and size
@ren.AddActor(outlineActor)
@ren.AddActor(actor)
@renWin.set_size_request(500, 500)
@ren.SetBackground(0.1, 0.1, 0.2)

=begin
tprop = Vtk::TextProperty.new
tprop.SetColor(1,1,1)
axes = Vtk::CubeAxesActor2D.new
axes.SetInput(grid)
axes.SetCamera(@ren.GetActiveCamera)
#axes.SetLabelFormat("%4.2f")
#axes.SetFlyModeToOuterEdges
#axes.SetFlyModeToClosestTriad
axes.SetFontFactor(1.2)
axes.SetAxisTitleTextProperty(tprop)
axes.SetAxisLabelTextProperty(tprop)
axes.UseRangesOn
axes.ScalingOff
@ren.AddProp(axes)
=end

# Create the GUI
# We first create the supporting functions (callbacks) for the GUI

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
 

        
def SetSlice(adj)
  @surface.SetValue(0,adj.value)
  @ren.ResetCameraClippingRange()
  @renWin.Render()
end




# Buttons
ctrl_buttons = Gtk::HBox.new

quit_button = Gtk::Button.new("Quit")
quit_button.signal_connect("clicked"){ Gtk.main_quit }
capture_button = Gtk::Button.new("PNG")
capture_button.signal_connect("clicked"){ captureImage }
ctrl_buttons.pack_start(quit_button)
ctrl_buttons.pack_start(capture_button)
top.pack_start(ctrl_buttons)



# Add a slice scale to browse the current slice stack

slice = Gtk::HScale.new(min,max,(max-min)/100)
slice.value = (max+min)/2
@adjust = slice.adjustment
@adjust.signal_connect("value-changed"){|w| SetSlice(w) }
top.pack_start(slice)

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