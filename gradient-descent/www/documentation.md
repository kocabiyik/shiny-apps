## Gradient Descent Algorithm

Gradient Descent is an iterative algorithm to solve optimization problems. It is frequently used for solving many Machine Learning problems.  

This app is an example for linear regression.  

### The Outline
We have the following hypothesis:  
$$h_\theta(x) = \theta_0+\theta_1 x$$

It represents a linear relationship. To find the parameters, we need to define a cost function as follows:  

$$J(\theta_0, \theta_1) = \frac{1}{2m}\sum\limits_1^m(h_\theta(x^i)-y^i)^2$$  

When we find the minimum value of $J$, we also find the parameters for the best fit.  

### The Algorithm
We will start with some random parameters. Then we will simulteneously update the parameters when we find the mininum $J$.

repeat until convergence:
$$\theta_j := \theta_j-\alpha\frac{\partial}{\partial_j}J(\theta_0,\theta_1)$$ for i = 0 and i = 1

### How to use the app
1. Select initial parameter values (Optional)
2. Select a learning rate (it will converge faster for higher values).
3. Set number of iterations (How many times are we going to update our parameters?)  