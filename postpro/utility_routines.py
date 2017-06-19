import vtk
import numpy as np
def end_max(reader, scale, number_of_divisions=40):
    """ 
    Retrieve the end forces and the maximum force within the span
    (if exists) and produce the related hedgehogs
    """
    # Initialize text dictionary
    text_dict = {}
    #
    out = reader.GetOutput()
    points = out.GetPoints()
    point_data = out.GetPointData()
    vectors = point_data.GetVectors()

    # Get number of points
    number_of_points = points.GetNumberOfPoints()

    # Allocate arrays
    coordinates = np.zeros((number_of_points, 3))
    internal_force = np.zeros((number_of_points, 3))

    # Fill the coordinates array
    for i in range(number_of_points):
        coordinates[i,:] = points.GetPoint(i)
        internal_force[i,:] = vectors.GetTuple(i)

    # Prepare the lists
    force_ends = []
    force_maxs = []

    # Get the ends first
    i = 0
    while i < number_of_points:
        force_ends.append( (internal_force[i,:], coordinates[i,:]) )
        force_ends.append( (internal_force[i+40,:], coordinates[i+40,:]) )
        i = i + 41

    # Write the texts for end forces
    for i in range(len(force_ends)):
        tta = vtk.vtkTextActor()
        tta.SetVisibility(0)
        magn = np.sqrt(force_ends[i][0][0]**2 + force_ends[i][0][1]**2)
        text = "%.2f" %magn
        tta.SetInput(text)
        tp = tta.GetTextProperty()
        tp.SetFontSize(16)
        tp.SetBold(1)
        tta.GetPositionCoordinate().SetCoordinateSystemToWorld()
        tta.SetPosition(force_ends[i][1][0] 
                        + scale*force_ends[i][0][0], 
                        force_ends[i][1][1] 
                        + scale*force_ends[i][0][1])
        text_dict[tta] = force_ends[i]
    # Get the maximum values
    j = 0 
    max_force = 0
    for i in range(number_of_points):
        # Convert vectors to scalar (magnitude)
        current_force = np.sqrt(internal_force[i,0]**2 
                                + internal_force[i,1]**2 
                                + internal_force[i,2]**2)
        # Update maximum values
        if current_force >= max_force:
            max_force = current_force
            max_force_vector = [ internal_force[i,:], i ]

        # Add tuples to the lists and reset when the element ends
        # 0-40, 41-81, 82-122, ... >> j = 41, 82, 123, ...
        if ( (i != j) and ( (i-j) % number_of_divisions ) == 0):
            j = i+1
            force_maxs.append( (max_force_vector[0], 
                                coordinates[max_force_vector[1]]) )
            max_force = 0
            max_force_vector = 0

    # Max force text
    for i in range(len(force_maxs)):
        tta = vtk.vtkTextActor()
        tta.SetVisibility(0)
        magn = np.sqrt(force_maxs[i][0][0]**2 + force_maxs[i][0][1]**2)
        text = "%.2f" %magn
        tta.SetInput(text)
        tp = tta.GetTextProperty()
        tp.SetFontSize(16)
        tp.SetBold(1)
        tta.GetPositionCoordinate().SetCoordinateSystemToWorld()
        tta.SetPosition(force_maxs[i][1][0] 
                        + scale*force_maxs[i][0][0], 
                        force_maxs[i][1][1] 
                        + scale*force_maxs[i][0][1])
        text_dict[tta] = force_maxs[i]
    #
    # Write data to vtk file, then read from it (fix this)
    outfile = open("/tmp/tmp.vtk", 'w')
    outfile.write("# vtk DataFile Version 2.0\n")
    outfile.write("temporary_end output\n")
    outfile.write("ASCII\n")
    outfile.write("DATASET UNSTRUCTURED_GRID\n")
    npoints = len(force_ends)
    outfile.write("POINTS %d float\n" %npoints)
    for i in range(len(force_ends)):
        outfile.write("%24.16f %24.16f %24.16f\n"
                      %(force_ends[i][1][0], force_ends[i][1][1],
                        force_ends[i][1][2]) )
    outfile.write("\nCELLS %d %d\n" %(npoints-1, (npoints-1)*3) )
    for i in range(npoints-1):
        outfile.write("%d %d %d\n" %(2, i, i+1) )
    outfile.write("\nCELL_TYPES %d \n" %(npoints-1) )
    for i in range(npoints-1):
        outfile.write("%d\n" %(3) )
    outfile.write("\nPOINT_DATA %d\n" %npoints)
    outfile.write("VECTORS tmp float\n")
    for i in range(len(force_ends)):
        outfile.write("%24.16f %24.16f %24.16f\n"
                      %(force_ends[i][0][0], force_ends[i][0][1],
                        force_ends[i][0][2]))
    outfile.close()

    outfile = open("/tmp/tmp2.vtk", 'w')
    outfile.write("# vtk DataFile Version 2.0\n")
    outfile.write("temporary_max output\n")
    outfile.write("ASCII\n")
    outfile.write("DATASET UNSTRUCTURED_GRID\n")
    npoints = len(force_maxs)
    outfile.write("POINTS %d float\n" %npoints)
    for i in range(len(force_maxs)):
        outfile.write("%24.16f %24.16f %24.16f\n"
                      %(force_maxs[i][1][0], force_maxs[i][1][1],
                        force_maxs[i][1][2]))
    outfile.write("\nCELLS %d %d\n" %(npoints-1, (npoints-1)*3) )
    for i in range(npoints-1):
        outfile.write("%d %d %d\n" %(2, i, i+1) )
    outfile.write("\nCELL_TYPES %d \n" %(npoints-1) )
    for i in range(npoints-1):
        outfile.write("%d\n" %(3) )

    outfile.write("\nPOINT_DATA %d\n" %npoints)
    outfile.write("VECTORS tmp float\n")
    for i in range(len(force_maxs)):
        outfile.write("%24.16f %24.16f %24.16f\n"
                      %(force_maxs[i][0][0], force_maxs[i][0][1],
                        force_maxs[i][0][2]))

    outfile.close()
    # Create the end & maximum hedgehogs
    max_reader = vtk.vtkUnstructuredGridReader()
    max_reader.SetFileName("/tmp/tmp2.vtk")
    max_reader.ReadAllVectorsOn()
    max_reader.Update()
    maximum_hedgehog = vtk.vtkHedgeHog()
    maximum_hedgehog.SetInput(max_reader.GetOutput())
    maximum_hedgehog_mapper = vtk.vtkPolyDataMapper()
    maximum_hedgehog_mapper.SetInputConnection(maximum_hedgehog.GetOutputPort())
    max_actor = vtk.vtkActor()
    max_actor.SetMapper(maximum_hedgehog_mapper)

    end_reader = vtk.vtkUnstructuredGridReader()
    end_reader.SetFileName("/tmp/tmp.vtk")
    end_reader.ReadAllVectorsOn()
    end_reader.Update()
    end_hedgehog = vtk.vtkHedgeHog()
    end_hedgehog.SetInput(end_reader.GetOutput())
    end_hedgehog_mapper = vtk.vtkPolyDataMapper()
    end_hedgehog_mapper.SetInputConnection(end_hedgehog.GetOutputPort())
    end_actor = vtk.vtkActor()
    end_actor.SetMapper(end_hedgehog_mapper)

    return maximum_hedgehog, max_actor, end_hedgehog, end_actor, text_dict

def get_scale(reader):
    out = reader.GetOutput()
    point = out.GetPoints()
    coord = point.GetData()
    max_coord, min_coord = coord.GetRange()
    data = out.GetPointData()
    vectors = data.GetVectors()
    max_magnitude = 0
    for i in range(point.GetNumberOfPoints()):
        cv = vectors.GetTuple(i)
        current_magnitude = np.sqrt(cv[0]**2 + cv[1]**2)
        if current_magnitude > max_magnitude:
            max_magnitude = current_magnitude
    coord_range = abs(max_coord - min_coord)
    if max_magnitude != 0:
        scale = coord_range / max_magnitude*0.2
    else:
        scale = 0.5
    return scale
    if __name__ == "__main__":
        pass

def max_disp(reader):
    out = reader.GetOutput()
    points = out.GetPoints()
    data = out.GetPointData()
    vectors = data.GetVectors()
    u_max = 0
    v_max = 0
    u_ind = 0 
    v_ind = 0
    for i in range(points.GetNumberOfPoints()):
        u_current = vectors.GetTuple(i)[0]
        v_current = vectors.GetTuple(i)[1]
        if abs(u_current) > abs(u_max):
            u_max = u_current
            u_ind = i
        if abs(v_current) > abs(v_max):
            v_max = v_current
            v_ind = i
    max_text_actor = vtk.vtkTextActor()
    u_max_position = out.GetPoint(u_ind)
    v_max_position = out.GetPoint(v_ind)
    max_text = """
u_max = %.5f mm \n@ (%.2f, %.2f)\n
v_max = %.5f mm \n@ (%.2f, %.2f)
              """ %(u_max*100, u_max_position[0], u_max_position[1],
                    v_max*100, v_max_position[0], v_max_position[1])
    max_text_actor.SetInput(max_text)
    max_text_actor.SetVisibility(0)
    pos = max_text_actor.GetPositionCoordinate()
    pos.SetCoordinateSystemToNormalizedDisplay()
    max_text_actor.SetPosition(0.8, 0.5)
    prop = max_text_actor.GetTextProperty()
    prop.SetFontSize(16)
    return max_text_actor


