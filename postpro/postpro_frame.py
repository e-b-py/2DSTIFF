import vtk
from utility_routines import end_max, get_scale, max_disp
import numpy as np

# Number of divisions for each element
number_of_divisions = 40

# -------------------------------------------------------------------
# OBSERVER FUNCTIONS FOR INTERACTION
# -------------------------------------------------------------------
def update_text_position(text_dict, scale):
    actors = text_dict.keys()
    for i in range(len(actors)):
        posx = text_dict[actors[i]][1][0] + text_dict[actors[i]][0][0]*scale
        posy = text_dict[actors[i]][1][1] + text_dict[actors[i]][0][1]*scale
        actors[i].SetPosition(posx, posy)
    return

    return
def deformed_slider_callback(obj, event):
    warp_deformed.SetScaleFactor(deformed_slider_rep.GetValue())
    return
def axial_slider_callback(obj, event):
    current_scale = axial_slider_rep.GetValue()
    warp_axial.SetScaleFactor(current_scale)
    hedge_axial.SetScaleFactor(current_scale)
    axial_max_hogs.SetScaleFactor(current_scale)
    axial_end_hogs.SetScaleFactor(current_scale)
    update_text_position(axial_text_dict, current_scale)
    return
def shear_slider_callback(obj, event):
    current_scale = shear_slider_rep.GetValue()
    warp_shear.SetScaleFactor(current_scale)
    hedge_shear.SetScaleFactor(current_scale)
    shear_max_hogs.SetScaleFactor(current_scale)
    shear_end_hogs.SetScaleFactor(current_scale)
    update_text_position(shear_text_dict, current_scale)
    return
def moment_slider_callback(obj, event):
    current_scale = moment_slider_rep.GetValue()
    warp_moment.SetScaleFactor(current_scale)
    hedge_moment.SetScaleFactor(current_scale)
    moment_max_hogs.SetScaleFactor(current_scale)
    moment_end_hogs.SetScaleFactor(current_scale)
    update_text_position(moment_text_dict, current_scale)
    return
def toggle_visibility(obj, event):
    """
    1: undeformed configuration
    2: deformed shape
    3: axial force diagram
    4: shear force diagram
    5: bending moment diagram
    f: fill diagram
    """
    code = interactor.GetKeyCode()
    if code == "1":
        undeformed_actor.SetVisibility(1)
        warp_deformed_actor.SetVisibility(0)
        warp_axial_actor.SetVisibility(0)
        hedge_axial_actor.SetVisibility(0)
        axial_max_actor.SetVisibility(0)
        axial_end_actor.SetVisibility(0)
        warp_shear_actor.SetVisibility(0)
        hedge_shear_actor.SetVisibility(0)
        shear_max_actor.SetVisibility(0)
        shear_end_actor.SetVisibility(0)
        warp_moment_actor.SetVisibility(0)
        hedge_moment_actor.SetVisibility(0)
        moment_max_actor.SetVisibility(0)
        moment_end_actor.SetVisibility(0)
        #
        deformed_slider_rep.SetVisibility(0)
        axial_slider_rep.SetVisibility(0)
        shear_slider_rep.SetVisibility(0)
        moment_slider_rep.SetVisibility(0)
        #
        undeformed_actor.GetProperty().SetLineStipplePattern(0xffff)
        undeformed_actor.GetProperty().SetLineWidth(1.0)
        #
        deformed_slider.EnabledOff()
        axial_slider.EnabledOff()
        shear_slider.EnabledOff()
        moment_slider.EnabledOff()
        #
        for i in range(len(axial_text_keys)):
            axial_text_keys[i].SetVisibility(0)
            shear_text_keys[i].SetVisibility(0)
            moment_text_keys[i].SetVisibility(0)
        #
        max_disp_text.SetVisibility(0)
        diagram_text.SetInput("Original configuration")
        ren_win.Render()
    elif code == "2":
        undeformed_actor.SetVisibility(1)
        warp_deformed_actor.SetVisibility(1)
        warp_axial_actor.SetVisibility(0)
        hedge_axial_actor.SetVisibility(0)
        axial_max_actor.SetVisibility(0)
        axial_end_actor.SetVisibility(0)
        warp_shear_actor.SetVisibility(0)
        hedge_shear_actor.SetVisibility(0)
        shear_max_actor.SetVisibility(0)
        shear_end_actor.SetVisibility(0)
        warp_moment_actor.SetVisibility(0)
        hedge_moment_actor.SetVisibility(0)
        moment_max_actor.SetVisibility(0)
        moment_end_actor.SetVisibility(0)
        #
        deformed_slider_rep.SetVisibility(1)
        axial_slider_rep.SetVisibility(0)
        shear_slider_rep.SetVisibility(0)
        moment_slider_rep.SetVisibility(0)
        #
        undeformed_actor.GetProperty().SetLineStipplePattern(0x000f)
        undeformed_actor.GetProperty().SetLineStippleRepeatFactor(1)
        undeformed_actor.GetProperty().SetPointSize(1)
        undeformed_actor.GetProperty().SetLineWidth(2.0)
        warp_deformed_actor.GetProperty().SetLineWidth(3.0)
        #
        deformed_slider.EnabledOn()
        axial_slider.EnabledOff()
        shear_slider.EnabledOff()
        moment_slider.EnabledOff()
        #
        for i in range(len(axial_text_keys)):
            axial_text_keys[i].SetVisibility(0)
            shear_text_keys[i].SetVisibility(0)
            moment_text_keys[i].SetVisibility(0)
        #
        max_disp_text.SetVisibility(1)
        diagram_text.SetInput("Deformed shape")
        #
        ren_win.Render()
    elif code == "3":
        undeformed_actor.SetVisibility(1)
        warp_deformed_actor.SetVisibility(0)
        warp_axial_actor.SetVisibility(1)
        hedge_axial_actor.SetVisibility(0)
        axial_max_actor.SetVisibility(1)
        axial_end_actor.SetVisibility(1)
        warp_shear_actor.SetVisibility(0)
        hedge_shear_actor.SetVisibility(0)
        shear_max_actor.SetVisibility(0)
        shear_end_actor.SetVisibility(0)
        warp_moment_actor.SetVisibility(0)
        hedge_moment_actor.SetVisibility(0)
        moment_max_actor.SetVisibility(0)
        moment_end_actor.SetVisibility(0)
        #
        deformed_slider_rep.SetVisibility(0)
        axial_slider_rep.SetVisibility(1)
        shear_slider_rep.SetVisibility(0)
        moment_slider_rep.SetVisibility(0)
        #
        undeformed_actor.GetProperty().SetLineStipplePattern(0xffff)
        undeformed_actor.GetProperty().SetLineWidth(4.0)
        #
        deformed_slider.EnabledOff()
        axial_slider.EnabledOn()
        shear_slider.EnabledOff()
        moment_slider.EnabledOff()
        #
        for i in range(len(axial_text_keys)):
            axial_text_keys[i].SetVisibility(0)
            shear_text_keys[i].SetVisibility(0)
            moment_text_keys[i].SetVisibility(0)
        #
        max_disp_text.SetVisibility(0)
        diagram_text.SetInput("Axial force diagram")
        ren_win.Render()
    elif code == "4":
        undeformed_actor.SetVisibility(1)
        warp_deformed_actor.SetVisibility(0)
        warp_axial_actor.SetVisibility(0)
        hedge_axial_actor.SetVisibility(0)
        axial_max_actor.SetVisibility(0)
        axial_end_actor.SetVisibility(0)
        warp_shear_actor.SetVisibility(1)
        hedge_shear_actor.SetVisibility(0)
        shear_max_actor.SetVisibility(1)
        shear_end_actor.SetVisibility(1)
        warp_shear_actor.SetVisibility(1)
        warp_moment_actor.SetVisibility(0)
        hedge_moment_actor.SetVisibility(0)
        moment_max_actor.SetVisibility(0)
        moment_end_actor.SetVisibility(0)
        warp_shear_actor.SetVisibility(1)
        #
        deformed_slider_rep.SetVisibility(0)
        axial_slider_rep.SetVisibility(0)
        shear_slider_rep.SetVisibility(1)
        moment_slider_rep.SetVisibility(0)
        #
        undeformed_actor.GetProperty().SetLineStipplePattern(0xffff)
        undeformed_actor.GetProperty().SetLineWidth(4.0)
        #
        deformed_slider.EnabledOff()
        axial_slider.EnabledOff()
        shear_slider.EnabledOn()
        moment_slider.EnabledOff()
        #
        for i in range(len(axial_text_keys)):
            axial_text_keys[i].SetVisibility(0)
            shear_text_keys[i].SetVisibility(0)
            moment_text_keys[i].SetVisibility(0)
        #
        max_disp_text.SetVisibility(0)
        diagram_text.SetInput("Shear force diagram")
        #
        for key in moment_text_keys:
            key.SetVisibility(0)
        ren_win.Render()
    elif code == "5":
        undeformed_actor.SetVisibility(1)
        warp_deformed_actor.SetVisibility(0)
        warp_axial_actor.SetVisibility(0)
        hedge_axial_actor.SetVisibility(0)
        axial_max_actor.SetVisibility(0)
        axial_end_actor.SetVisibility(0)
        warp_shear_actor.SetVisibility(0)
        hedge_shear_actor.SetVisibility(0)
        shear_max_actor.SetVisibility(0)
        shear_end_actor.SetVisibility(0)
        warp_moment_actor.SetVisibility(1)
        hedge_moment_actor.SetVisibility(0)
        moment_max_actor.SetVisibility(1)
        moment_end_actor.SetVisibility(1)
        #
        deformed_slider_rep.SetVisibility(0)
        axial_slider_rep.SetVisibility(0)
        shear_slider_rep.SetVisibility(0)
        moment_slider_rep.SetVisibility(1)
        #
        undeformed_actor.GetProperty().SetLineStipplePattern(0xffff)
        undeformed_actor.GetProperty().SetLineWidth(4.0)
        #
        deformed_slider.EnabledOff()
        axial_slider.EnabledOff()
        shear_slider.EnabledOff()
        moment_slider.EnabledOn()
        #
        for i in range(len(axial_text_keys)):
            axial_text_keys[i].SetVisibility(0)
            shear_text_keys[i].SetVisibility(0)
            moment_text_keys[i].SetVisibility(0)
        #
        max_disp_text.SetVisibility(0)
        diagram_text.SetInput("Bending moment diagram")
        #
        ren_win.Render()

    elif code == "f":
        if deformed_slider_rep.GetVisibility():
            pass
        if axial_slider_rep.GetVisibility():
            if hedge_axial_actor.GetVisibility():
                hedge_axial_actor.SetVisibility(0) 
            else:
                hedge_axial_actor.SetVisibility(1)
        if shear_slider_rep.GetVisibility():
            if hedge_shear_actor.GetVisibility():
                hedge_shear_actor.SetVisibility(0) 
            else:
                hedge_shear_actor.SetVisibility(1)
        if moment_slider_rep.GetVisibility():
            if hedge_moment_actor.GetVisibility():
                hedge_moment_actor.SetVisibility(0) 
            else:
                hedge_moment_actor.SetVisibility(1)
        ren_win.Render()

    elif code == "v":
        if axial_slider_rep.GetVisibility():
            if axial_text_keys[0].GetVisibility():
                for i in range(len(axial_text_keys)):
                    axial_text_keys[i].SetVisibility(0)
            else:
                for i in range(len(axial_text_keys)):
                    axial_text_keys[i].SetVisibility(1)
        if shear_slider_rep.GetVisibility():
            if shear_text_keys[0].GetVisibility():
                for i in range(len(shear_text_keys)):
                    shear_text_keys[i].SetVisibility(0)
            else:
                for i in range(len(shear_text_keys)):
                    shear_text_keys[i].SetVisibility(1)
        if moment_slider_rep.GetVisibility():
            if moment_text_keys[0].GetVisibility():
                for i in range(len(moment_text_keys)):
                    moment_text_keys[i].SetVisibility(0)
            else:
                for i in range(len(moment_text_keys)):
                    moment_text_keys[i].SetVisibility(1)

        ren_win.Render()
    elif code == "r":
        camera.SetPosition(camera_position)
        camera.SetRoll(0.0)
        ren_win.Render()
    else:
        pass
    return

# -------------------------------------------------------------------
# READERS
# -------------------------------------------------------------------

# UNDEFORMED 
undeformed_reader = vtk.vtkUnstructuredGridReader()
undeformed_reader.SetFileName('../out/undeformed.vtk')
undeformed_reader.Update()
undeformed_mapper = vtk.vtkDataSetMapper()
undeformed_mapper.SetInput(undeformed_reader.GetOutput())
undeformed_actor = vtk.vtkActor()
undeformed_actor.SetMapper(undeformed_mapper)
# DEFORMED
deformed_reader = vtk.vtkUnstructuredGridReader()
deformed_reader.SetFileName('../out/deformed.vtk')
deformed_reader.ReadAllVectorsOn()
deformed_reader.Update()
# AXIAL
axial_reader = vtk.vtkUnstructuredGridReader()
axial_reader.SetFileName('../out/axial.vtk')
axial_reader.ReadAllVectorsOn()
axial_reader.Update()
# SHEAR
shear_reader = vtk.vtkUnstructuredGridReader()
shear_reader.SetFileName('../out/shear.vtk')
shear_reader.ReadAllVectorsOn()
shear_reader.Update()
# MOMENT
moment_reader = vtk.vtkUnstructuredGridReader()
moment_reader.SetFileName('../out/moment.vtk')
moment_reader.ReadAllVectorsOn()
moment_reader.Update()

# ---------------------------------------------------
# GET THE INITIAL SCALES FOR WARP VECTORS
# ---------------------------------------------------

axial_scale = get_scale(axial_reader)
shear_scale = get_scale(shear_reader)
moment_scale = get_scale(moment_reader)

# ---------------------------------------------------
# WARP VECTORS
# ---------------------------------------------------

# DEFORMED
warp_deformed = vtk.vtkWarpVector()
warp_deformed.SetInput(deformed_reader.GetOutput())
warp_deformed.SetScaleFactor(50)
warp_deformed.Update()
warp_deformed_mapper = vtk.vtkDataSetMapper()
warp_deformed_mapper.SetInput(warp_deformed.GetOutput())
warp_deformed_actor = vtk.vtkActor()
warp_deformed_actor.SetMapper(warp_deformed_mapper)
warp_deformed_actor.SetVisibility(0)
# AXIAL
warp_axial = vtk.vtkWarpVector()
warp_axial.SetInput(axial_reader.GetOutput())
warp_axial.SetScaleFactor(axial_scale)
warp_axial.Update()
warp_axial_mapper = vtk.vtkDataSetMapper()
warp_axial_mapper.SetInput(warp_axial.GetOutput())
warp_axial_actor = vtk.vtkActor()
warp_axial_actor.SetMapper(warp_axial_mapper)
warp_axial_actor.SetVisibility(0)
# SHEAR
warp_shear = vtk.vtkWarpVector()
warp_shear.SetInput(shear_reader.GetOutput())
warp_shear.SetScaleFactor(shear_scale)
warp_shear.Update()
warp_shear_mapper = vtk.vtkDataSetMapper()
warp_shear_mapper.SetInput(warp_shear.GetOutput())
warp_shear_actor = vtk.vtkActor()
warp_shear_actor.SetMapper(warp_shear_mapper)
warp_shear_actor.SetVisibility(0)
# MOMENT
warp_moment = vtk.vtkWarpVector()
warp_moment.SetInput(moment_reader.GetOutput())
warp_moment.SetScaleFactor(moment_scale)
warp_moment.Update()
warp_moment_mapper = vtk.vtkDataSetMapper()
warp_moment_mapper.SetInput(warp_moment.GetOutput())
warp_moment_actor = vtk.vtkActor()
warp_moment_actor.SetMapper(warp_moment_mapper)
warp_moment_actor.SetVisibility(0)

# ---------------------------------------------------
# HEDGEHOGS
# ---------------------------------------------------

# AXIAL
hedge_axial = vtk.vtkHedgeHog()
hedge_axial.SetInput(axial_reader.GetOutput())
hedge_axial.SetScaleFactor(axial_scale)
hedge_axial_mapper = vtk.vtkPolyDataMapper()
hedge_axial_mapper.SetInputConnection(hedge_axial.GetOutputPort())
hedge_axial_actor = vtk.vtkActor()
hedge_axial_actor.SetMapper(hedge_axial_mapper)
hedge_axial_actor.SetVisibility(0)
# SHEAR
hedge_shear = vtk.vtkHedgeHog()
hedge_shear.SetInput(shear_reader.GetOutput())
hedge_shear.SetScaleFactor(shear_scale)
hedge_shear_mapper = vtk.vtkPolyDataMapper()
hedge_shear_mapper.SetInputConnection(hedge_shear.GetOutputPort())
hedge_shear_actor = vtk.vtkActor()
hedge_shear_actor.SetMapper(hedge_shear_mapper)
hedge_shear_actor.SetVisibility(0)
# MOMENT
hedge_moment = vtk.vtkHedgeHog()
hedge_moment.SetInput(moment_reader.GetOutput())
hedge_moment.SetScaleFactor(moment_scale)
hedge_moment_mapper = vtk.vtkPolyDataMapper()
hedge_moment_mapper.SetInputConnection(hedge_moment.GetOutputPort())
hedge_moment_actor = vtk.vtkActor()
hedge_moment_actor.SetMapper(hedge_moment_mapper)
hedge_moment_actor.SetVisibility(0)

# -------------------------------------------------------------------
# GET END & MAXIMUM HEDGEHOGS
# -------------------------------------------------------------------

axial_max_hogs, axial_max_actor, axial_end_hogs, axial_end_actor, \
    axial_text_dict = end_max(axial_reader, axial_scale, number_of_divisions)
shear_max_hogs, shear_max_actor, shear_end_hogs, shear_end_actor, \
    shear_text_dict = end_max(shear_reader, shear_scale,  number_of_divisions)
moment_max_hogs, moment_max_actor, moment_end_hogs, moment_end_actor, \
    moment_text_dict = end_max(moment_reader, moment_scale, number_of_divisions)

axial_max_actor.SetVisibility(0)
axial_end_actor.SetVisibility(0)
axial_max_hogs.SetScaleFactor(axial_scale)
axial_end_hogs.SetScaleFactor(axial_scale)
shear_max_actor.SetVisibility(0)
shear_end_actor.SetVisibility(0)
shear_max_hogs.SetScaleFactor(shear_scale)
shear_end_hogs.SetScaleFactor(shear_scale)
moment_max_actor.SetVisibility(0)
moment_end_actor.SetVisibility(0)
moment_max_hogs.SetScaleFactor(moment_scale)
moment_end_hogs.SetScaleFactor(moment_scale)

# -------------------------------------------------------------------
# ADD THE TEXTS
# -------------------------------------------------------------------

explain_text = vtk.vtkTextActor()
explain_text.SetInput("""
Toggle Diagrams

1: Original configuration
                            
2: Deformed shape           
                            
3: Axial force              
                            
4: Shear force              
                            
5: Bending moment           

f: Fill diagram

v: Show values

r: Reset camera
                      """)
explain_text.GetTextProperty().SetFontSize(16)
explain_text.GetPositionCoordinate().SetCoordinateSystemToNormalizedDisplay()
explain_text.SetPosition(0.02, 0.30)

diagram_text = vtk.vtkTextActor()
diagram_text.SetInput("Original configuration")
diagram_text.GetTextProperty().SetFontSize(22)
diagram_text.GetTextProperty().SetBold(1)
diagram_text.GetPositionCoordinate().SetCoordinateSystemToNormalizedDisplay()
diagram_text.SetPosition(0.03, 0.8)

max_disp_text = max_disp(deformed_reader)
# -------------------------------------------------------------------
# RENDERER AND INTERACTOR
# -------------------------------------------------------------------

renderer = vtk.vtkRenderer()
renderer.AddActor(undeformed_actor)
renderer.AddActor(warp_deformed_actor)
renderer.AddActor(warp_axial_actor)
renderer.AddActor(warp_shear_actor)
renderer.AddActor(warp_moment_actor)
renderer.AddActor(hedge_axial_actor)
renderer.AddActor(hedge_shear_actor)
renderer.AddActor(hedge_moment_actor)
renderer.AddActor(axial_max_actor)
renderer.AddActor(axial_end_actor)
renderer.AddActor(shear_max_actor)
renderer.AddActor(shear_end_actor)
renderer.AddActor(moment_max_actor)
renderer.AddActor(moment_end_actor)
renderer.AddActor(explain_text)
renderer.AddActor(diagram_text)
renderer.AddActor(max_disp_text)

axial_text_keys = axial_text_dict.keys()
axial_text_vals = axial_text_dict.values()
shear_text_keys = shear_text_dict.keys()
shear_text_vals = shear_text_dict.values()
moment_text_keys = moment_text_dict.keys()
moment_text_vals = moment_text_dict.values()
for i in range(len(moment_text_keys)):
    renderer.AddActor(axial_text_keys[i])
    renderer.AddActor(shear_text_keys[i])
    renderer.AddActor(moment_text_keys[i])
camera = renderer.GetActiveCamera()
renderer.ResetCamera()
camera_position = camera.GetPosition()

ren_win = vtk.vtkRenderWindow()
ren_win.SetSize(1200, 800)
ren_win.AddRenderer(renderer)
#
interactor = vtk.vtkRenderWindowInteractor()
style = vtk.vtkInteractorStyleTrackballCamera()
interactor.SetInteractorStyle(style)
interactor.SetRenderWindow(ren_win)
#
#interactor.RemoveAllObservers()
#interactor.AddObserver("LeftButtonPressEvent", left_mouse_button)
#
ren_win.Render()

# -------------------------------------------------------------------
# SLIDERS REPRESENTATIONS
# -------------------------------------------------------------------

# DEFORMED
deformed_slider_rep = vtk.vtkSliderRepresentation2D()
deformed_slider_rep.SetMinimumValue(0.0)
deformed_slider_rep.SetMaximumValue(1000.0)
deformed_slider_rep.SetValue(50.0)
deformed_slider_rep.SetTitleText("Displacement Scale")
deformed_slider_rep.SetVisibility(0)
# Position it
deformed_slider_rep.GetPoint1Coordinate().SetCoordinateSystemToNormalizedDisplay()
deformed_slider_rep.GetPoint1Coordinate().SetValue(0.04, 0.2)
deformed_slider_rep.GetPoint2Coordinate().SetCoordinateSystemToNormalizedDisplay()
deformed_slider_rep.GetPoint2Coordinate().SetValue(0.14, 0.2)
# Colors
deformed_slider_rep.GetSliderProperty().SetColor(1.0, 1.0, 1.0)
deformed_slider_rep.GetTitleProperty().SetColor(1.0, 1.0, 1.0)
deformed_slider_rep.GetLabelProperty().SetColor(1.0, 1.0, 1.0)
# Actual widget
deformed_slider = vtk.vtkSliderWidget()
deformed_slider.SetInteractor(interactor)
deformed_slider.SetRepresentation(deformed_slider_rep)
deformed_slider.EnabledOn()
deformed_slider.AddObserver(vtk.vtkCommand.InteractionEvent, deformed_slider_callback)

# AXIAL
axial_slider_rep = vtk.vtkSliderRepresentation2D()
axial_slider_rep.SetMinimumValue(0.0)
axial_slider_rep.SetMaximumValue(axial_scale*2)
axial_slider_rep.SetValue(axial_scale)
axial_slider_rep.SetTitleText("Axial force scale")
axial_slider_rep.SetVisibility(0)
# Position it
axial_slider_rep.GetPoint1Coordinate().SetCoordinateSystemToNormalizedDisplay()
axial_slider_rep.GetPoint1Coordinate().SetValue(0.04, 0.2)
axial_slider_rep.GetPoint2Coordinate().SetCoordinateSystemToNormalizedDisplay()
axial_slider_rep.GetPoint2Coordinate().SetValue(0.14, 0.2)
# Colors
axial_slider_rep.GetSliderProperty().SetColor(1.0, 1.0, 1.0)
axial_slider_rep.GetTitleProperty().SetColor(1.0, 1.0, 1.0)
axial_slider_rep.GetLabelProperty().SetColor(1.0, 1.0, 1.0)
# Actual widget
axial_slider = vtk.vtkSliderWidget()
axial_slider.SetInteractor(interactor)
axial_slider.SetRepresentation(axial_slider_rep)
axial_slider.EnabledOn()
axial_slider.AddObserver(vtk.vtkCommand.InteractionEvent, axial_slider_callback)

# SHEAR
shear_slider_rep = vtk.vtkSliderRepresentation2D()
shear_slider_rep.SetMinimumValue(0.0)
shear_slider_rep.SetMaximumValue(shear_scale*2)
shear_slider_rep.SetValue(shear_scale)
shear_slider_rep.SetTitleText("Shear force scale")
shear_slider_rep.SetVisibility(0)
# Position it
shear_slider_rep.GetPoint1Coordinate().SetCoordinateSystemToNormalizedDisplay()
shear_slider_rep.GetPoint1Coordinate().SetValue(0.04, 0.2)
shear_slider_rep.GetPoint2Coordinate().SetCoordinateSystemToNormalizedDisplay()
shear_slider_rep.GetPoint2Coordinate().SetValue(0.14, 0.2)
# Colors
shear_slider_rep.GetSliderProperty().SetColor(1.0, 1.0, 1.0)
shear_slider_rep.GetTitleProperty().SetColor(1.0, 1.0, 1.0)
shear_slider_rep.GetLabelProperty().SetColor(1.0, 1.0, 1.0)
# Actual widget
shear_slider = vtk.vtkSliderWidget()
shear_slider.SetInteractor(interactor)
shear_slider.SetRepresentation(shear_slider_rep)
shear_slider.EnabledOn()
shear_slider.AddObserver(vtk.vtkCommand.InteractionEvent, shear_slider_callback)

# MOMENT
moment_slider_rep = vtk.vtkSliderRepresentation2D()
moment_slider_rep.SetMinimumValue(0.0)
moment_slider_rep.SetMaximumValue(moment_scale*2)
moment_slider_rep.SetValue(moment_scale)
moment_slider_rep.SetTitleText("Moment scale")
moment_slider_rep.SetVisibility(0)
# Position it
moment_slider_rep.GetPoint1Coordinate().SetCoordinateSystemToNormalizedDisplay()
moment_slider_rep.GetPoint1Coordinate().SetValue(0.04, 0.2)
moment_slider_rep.GetPoint2Coordinate().SetCoordinateSystemToNormalizedDisplay()
moment_slider_rep.GetPoint2Coordinate().SetValue(0.14, 0.2)
# Colors
moment_slider_rep.GetSliderProperty().SetColor(1.0, 1.0, 1.0)
moment_slider_rep.GetTitleProperty().SetColor(1.0, 1.0, 1.0)
moment_slider_rep.GetLabelProperty().SetColor(1.0, 1.0, 1.0)
# Actual widget
moment_slider = vtk.vtkSliderWidget()
moment_slider.SetInteractor(interactor)
moment_slider.SetRepresentation(moment_slider_rep)
moment_slider.EnabledOn()
moment_slider.AddObserver(vtk.vtkCommand.InteractionEvent, moment_slider_callback)

# -------------------------------------------------------------------
# Add toggle visibility to the interactor and start main loop
# -------------------------------------------------------------------

interactor.AddObserver("KeyPressEvent", toggle_visibility)
interactor.Start()
