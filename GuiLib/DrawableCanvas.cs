using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Windows.Devices.Input;
using Windows.Foundation;
using Windows.UI;
using Windows.UI.Input;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Input;
using Windows.UI.Xaml.Media;
using Windows.UI.Xaml.Shapes;

namespace GuiLib
{
    public sealed class DrawableCanvas2 : Canvas

    { }
    public sealed class DrawableCanvas : Canvas
    {
        private uint _penID;
        private uint _touchID;
        private Point _previousContactPt;
        private Point currentContactPt;
        private double x1;
        private double y1;
        private double x2;
        private double y2;
        public DrawableCanvas()
        {
            this.PointerPressed += new PointerEventHandler(MyCanvas_PointerPressed);
            this.PointerMoved += new PointerEventHandler(MyCanvas_PointerMoved);
            this.PointerReleased += new PointerEventHandler(MyCanvas_PointerReleased);
            this.PointerExited += new PointerEventHandler(MyCanvas_PointerReleased);
        }


        #region PointerEvents
        private void MyCanvas_PointerReleased(object sender, PointerRoutedEventArgs e)
        {
            _touchID = 0;
            _penID = 0;

            e.Handled = true;
        }

        private void MyCanvas_PointerMoved(object sender, PointerRoutedEventArgs e)
        {
            if (e.Pointer.PointerId == _penID || e.Pointer.PointerId == _touchID)
            {
                PointerPoint pt = e.GetCurrentPoint(this);

                // Render a red line on the canvas as the pointer moves. 
                // Distance() is an application-defined function that tests
                // whether the pointer has moved far enough to justify 
                // drawing a new line.
                currentContactPt = pt.Position;
                x1 = _previousContactPt.X;
                y1 = _previousContactPt.Y;
                x2 = currentContactPt.X;
                y2 = currentContactPt.Y;

                if (Distance(x1, y1, x2, y2) > 2.0) // We need to developp this method now 
                {
                    Line line = new Line()
                    {
                        X1 = x1,
                        Y1 = y1,
                        X2 = x2,
                        Y2 = y2,
                        StrokeThickness = 4.0,
                        Stroke = new SolidColorBrush(Colors.Green)
                    };

                    _previousContactPt = currentContactPt;

                    // Draw the line on the canvas by adding the Line object as
                    // a child of the Canvas object.
                    this.Children.Add(line);

                }
            }

            else if (e.Pointer.PointerId == _touchID)
            {
                // Process touch input
            }

        }

        private double Distance(double x1, double y1, double x2, double y2)
        {
            double d = 0;
            d = Math.Sqrt(Math.Pow((x2 - x1), 2) + Math.Pow((y2 - y1), 2));
            return d;
        }

        private void MyCanvas_PointerPressed(object sender, PointerRoutedEventArgs e)
        {
            // Get information about the pointer location.
            PointerPoint pt = e.GetCurrentPoint(this);
            _previousContactPt = pt.Position;

            // Accept input only from a pen or mouse with the left button pressed. 
            PointerDeviceType pointerDevType = e.Pointer.PointerDeviceType;
            if (pointerDevType == PointerDeviceType.Pen ||
                    pointerDevType == PointerDeviceType.Mouse &&
                    pt.Properties.IsLeftButtonPressed)
            {
                // Pass the pointer information to the InkManager.
                _penID = pt.PointerId;

                e.Handled = true;
            }

            else if (pointerDevType == PointerDeviceType.Touch)
            {
                // Process touch input
                _touchID = pt.PointerId;

                e.Handled = true;
            }
        }

        #endregion    
    }
}
