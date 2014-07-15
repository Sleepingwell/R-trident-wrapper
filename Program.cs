/*   Copyright 2010 Simon Knapp
*
*    Licensed under the Apache License, Version 2.0 (the "License");
*    you may not use this file except in compliance with the License.
*    You may obtain a copy of the License at
*
*       http://www.apache.org/licenses/LICENSE-2.0
*
*    Unless required by applicable law or agreed to in writing, software
*    distributed under the License is distributed on an "AS IS" BASIS,
*    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
*    See the License for the specific language governing permissions and
*    limitations under the License.
*/

using System;
using System.Threading;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Python.Runtime;

namespace ConsoleApplication1
{
    // note that the locking and use of Monitor in this class does not imply full thread safety. For example, multiple calls to submit can
    // overwrite things that live in the R global namespace. Same is true about the Python namespace.
    //
    // There are probably nicer ways to do a lot of what happens in this class (there is stacks of boilerplate), but it is simply a proof of
    // concept at this stage
    class R
    {
        private static volatile R instance_ = null;
        private static readonly object padlock_ = new object();
        private PyObject R_;
        private PyObject rObjects_;

        public static R instance() {
            R res = instance_;
            if (res == null) 
            {
                lock(padlock_) {
                    res = instance_;
                    if (res == null) 
                    {
                        instance_ = res = new R();
                    }
                }
            }
            return instance_;
        }

        private IntPtr getLock() {
            Monitor.Enter(padlock_);
            return PythonEngine.AcquireLock();
        }

        private void releaseLock(IntPtr lck) {
            PythonEngine.ReleaseLock(lck);
            Monitor.Exit(padlock_);
        }

        private R() {
            if (!PythonEngine.IsInitialized) PythonEngine.Initialize(); // we don't have to worry about this in instance methods now.
            IntPtr lck = getLock();
            try
            {
                rObjects_ = PythonEngine.ImportModule("rpy2.robjects");
                R_ = rObjects_.GetAttr("r");
            }
            finally
            {
                releaseLock(lck);
            }
        }

        public void submit(string code) 
        {
            IntPtr lck = getLock();
            try
            {
                R_.InvokeMethod("__call__", new PyObject[] { new PyString(code) });
            }
            finally
            {
                releaseLock(lck);
            }
        }

        public PyObject makeIntVector(int[] ints)
        {
            PyObject res;
            IntPtr lck = getLock();
            try
            {
                PyList tmp = new PyList();
                foreach (int i in ints)
                {
                    tmp.Append(new PyInt(i));
                }
                res = rObjects_.InvokeMethod("IntVector", new PyObject[] { tmp });
            }
            finally
            {
                releaseLock(lck);
            }
            return res;
        }

        public PyObject makeDoubleVector(double[] doubles)
        {
            PyObject res;
            IntPtr lck = getLock();
            try
            {
                PyList tmp = new PyList();
                foreach (double i in doubles)
                {
                    tmp.Append(new PyFloat(i));
                }
                res = rObjects_.InvokeMethod("FloatVector", new PyObject[] { tmp });
            }
            finally
            {
                releaseLock(lck);
            }
            return res;
        }

        public PyObject makeStringVector(string[] strings)
        {
            PyObject res;
            IntPtr lck = getLock();
            try
            {
                PyList tmp = new PyList();
                foreach (string i in strings)
                {
                    tmp.Append(new PyString(i));
                }
                res = rObjects_.InvokeMethod("StrVector", new PyObject[] { tmp });
            }
            finally
            {
                releaseLock(lck);
            }
            return res;
        }

        public PyObject makeIntVector(int val)
        {
            PyObject res;
            IntPtr lck = getLock();
            try
            {
                PyList tmp = new PyList();
                tmp.Append(new PyInt(val));
                res = rObjects_.InvokeMethod("IntVector", new PyObject[] { tmp });
            }
            finally
            {
                releaseLock(lck);
            }
            return res;
        } 

        public PyObject makeDoubleVector(double val)
        {
            PyObject res;
            IntPtr lck = getLock();
            try
            {
                PyList tmp = new PyList();
                tmp.Append(new PyFloat(val));
                res = rObjects_.InvokeMethod("FloatVector", new PyObject[] { tmp });
            }
            finally
            {
                releaseLock(lck);
            }
            return res;
        }

        public PyObject makeStringVector(string val)
        {
            PyObject res;
            IntPtr lck = getLock();
            try
            {
                PyList tmp = new PyList();
                tmp.Append(new PyString(val));
                res = rObjects_.InvokeMethod("StrVector", new PyObject[] { tmp });
            }
            finally
            {
                releaseLock(lck);
            }
            return res;
        }

        public PyObject doCall(string method, PyObject[] args)
        {
            PyObject res;
            IntPtr lck = getLock();
            try
            {
                res = R_.InvokeMethod(method, args);
            }
            finally
            {
                releaseLock(lck);
            }
            return res;
        }

        public PyObject makePyObject(object obj)
        {
            if (obj.GetType() == typeof(int)) return makeIntVector((int)obj);
            if (obj.GetType() == typeof(int[])) return makeIntVector((int[])obj);
            if (obj.GetType() == typeof(double)) return makeDoubleVector((double)obj);
            if (obj.GetType() == typeof(double[])) return makeDoubleVector((double[])obj);
            if (obj.GetType() == typeof(string)) return makeStringVector((string)obj);
            if (obj.GetType() == typeof(string[])) return makeStringVector((string[])obj);
            if (obj.GetType() == typeof(PyObject)) return (PyObject)obj;
            throw new Exception("Cannot convert " + obj.GetType().ToString() + " to PyObject");
        }
    }










 
    class ManagedObjectExtractor
    {
        private static readonly object padlock_ = new object();

        private static IntPtr getLock() {
            Monitor.Enter(padlock_);
            if (!PythonEngine.IsInitialized) PythonEngine.Initialize();
            return PythonEngine.AcquireLock();
        }

        private static void releaseLock(IntPtr lck) {
            if (!PythonEngine.IsInitialized) PythonEngine.Initialize(); // shouldn't be necessary
            PythonEngine.ReleaseLock(lck);
            Monitor.Exit(padlock_);
        }

        static public double[] asDoubleArray(PyObject result)
        {
            double[] res;
            IntPtr lck = getLock();
            try
            {
                res = (double[])result.AsManagedObject(typeof(double[]));
            }
            finally
            {
                releaseLock(lck);
            }
            return res;
        }

        static public double asDouble(PyObject result)
        {
            return asDoubleArray(result)[0];
        }

        static public int[] asIntArray(PyObject result) 
        {
            int[] res;
            IntPtr lck = getLock();
            try
            {
                res = ((int[])result.AsManagedObject(typeof(int[])));
            }
            finally
            {
                releaseLock(lck);
            }
            return res;
        }

        static public int asInt(PyObject result)
        {
            return asIntArray(result)[0];
        }

        static public string[] asStringArray(PyObject result)
        {
            string[] res;
            IntPtr lck = getLock();
            try
            {
                res = ((string[])result.AsManagedObject(typeof(string[])));
            }
            finally
            {
                releaseLock(lck);
            }
            return res;
        }

        static public string asString(PyObject result)
        {
            return asStringArray(result)[0];
        }

        static public PyObject asPyObject(PyObject result)
        {
            return result;
        }

        static public PyObject doExtract(PyObject result, int[] indexes)
        {
            PyObject last = result;
            IntPtr lck = getLock();
            try
            {
                foreach (int index in indexes)
                {
                    last = result[index];
                }
            }
            finally
            {
                releaseLock(lck);
            }
            return last;
        }
    }
   





    // I don't think we need to worry about locking to simply create PyObjects, PyLongs, ...
    // but I might be wrong.
    class rnorm
    {
        private PyObject result;
        private static string method = "rnorm";
        private PyObject[] args = new PyObject[1];

        public double[] Result {
            get
            {
                PyObject res = ManagedObjectExtractor.doExtract(result, new int[] {});
                return ManagedObjectExtractor.asDoubleArray(res);
            }
        }

        public int Length {
            set
            {
                args[0] = new PyLong(value);
            }
            get
            {
                return ManagedObjectExtractor.asInt(args[0]);
            }
        }

        public void Execute()
        {
            result = R.instance().doCall(method, args);
        }
    }







    class lm
    {
        private PyObject result;
        private static string method = "lmthingy";
        private PyObject[] args = new PyObject[]{};

        public int rank
        {
            get
            {
                PyObject res = ManagedObjectExtractor.doExtract(result, new int[] { 3 });
                return ManagedObjectExtractor.asInt(res);
            }
        }

        public void Execute()
        {
            R.instance().submit(method + "<-function(...) {x <- rnorm(100);y <- rnorm(100);mod <- lm(y~x)}");
            result = R.instance().doCall(method, args);
        }
    }







    class Program {
        static void testRnorm()
        {
            int len = 10;
            rnorm tester = new rnorm();
            tester.Length = len;
            tester.Execute();
            double[] result = tester.Result;
            for (int i = 0; i < result.Length; ++i)
            {
                Console.Out.WriteLine("val = " + result[i]);
            }
        }

        static void testLm()
        {
            lm tester = new lm();
            tester.Execute();
            int result = tester.rank;
            Console.Out.WriteLine("val = " + result);
        }

        static void testNP()
        {
            PyObject np = PythonEngine.ImportModule("numpy");
            PyObject x = np.InvokeMethod("arange", new PyObject[] { new PyInt(10) });
            double[] result = (double[])x.AsManagedObject(typeof(double[]));
            for (int i = 0; i < result.Length; ++i)
            {
                Console.Out.WriteLine("val = " + result[i]);
            }
        }

        static void Main(string[] args)
        {
            Console.Out.WriteLine("testing rnorm");
            testRnorm();
            Console.Out.WriteLine("testing lm");
            testLm();
            Console.Out.WriteLine("testing numpy");
            testNP();
            Console.Out.WriteLine("testing numpy 2 - should be much faster");
            testNP();
            PythonEngine.Shutdown();
        }
    }
}
