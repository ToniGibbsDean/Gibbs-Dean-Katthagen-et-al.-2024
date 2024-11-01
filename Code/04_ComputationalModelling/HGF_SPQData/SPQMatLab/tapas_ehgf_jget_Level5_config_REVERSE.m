function c = tapas_ehgf_jget_config % genpop
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Contains the configuration for the enhanced Hierarchical Gaussian Filter (eHGF) model for the
% jumping Gaussian estimation task (JGET).
%
% The HGF is the model introduced in 
%
% Mathys C, Daunizeau J, Friston, KJ, and Stephan KE. (2011). A Bayesian foundation
% for individual learning under uncertainty. Frontiers in Human Neuroscience, 5:39.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The HGF configuration consists of the priors of parameters and initial values. All priors are
% Gaussian in the space where the quantity they refer to is estimated. They are specified by their
% sufficient statistics: mean and variance (NOT standard deviation).
% 
% Quantities are estimated in their native space if they are unbounded (e.g., the omegas). They are
% estimated in log-space if they have a natural lower bound at zero (e.g., the sigmas).
% 
% Parameters can be fixed (i.e., set to a fixed value) by setting the variance of their prior to
% zero. Aside from being useful for model comparison, the need for this arises whenever the scale
% and origin at the j-th level are arbitrary. This is the case if the observation model does not
% contain the representations mu_j and sigma_j. A choice of scale and origin is then implied by
% fixing the initial value mu_j_0 of mu_j and either kappa_j-1 or omega_j-1.
%
% Fitted trajectories can be plotted by using the command
%
% >> tapas_ehgf_jget_plotTraj(est)
% 
% where est is the stucture returned by fitModel. This structure contains the estimated
% perceptual parameters in est.p_prc and the estimated trajectories of the agent's
% representations (cf. Mathys et al., 2011). Their meanings are:
%              
%         est.p_prc.mux_0      row vector of initial values of mu_x (in ascending order of levels)
%         est.p_prc.sax_0      row vector of initial values of sigma_x (in ascending order of levels)
%         est.p_prc.mua_0      row vector of initial values of mu_alpha (in ascending order of levels)
%         est.p_prc.saa_0      row vector of initial values of sigma_alpha (in ascending order of levels)
%         est.p_prc.kau        kappa_u
%         est.p_prc.omu        omega_u
%         est.p_prc.kax        row vector of kappa_x (in ascending order of levels)
%         est.p_prc.omx        row vector of omega_x (in ascending order of levels)
%         est.p_prc.kaa        row vector of kappa_alpha (in ascending order of levels)
%         est.p_prc.oma        row vector of omega_alpha (in ascending order of levels)
%
%         est.traj.mux         mux (rows: trials, columns: levels)
%         est.traj.sax         sigma_x (rows: trials, columns: levels)
%         est.traj.muxhat      prediction of mu_x (rows: trials, columns: levels)
%         est.traj.saxhat      precisions of predictions of x (rows: trials, columns: levels)
%         est.traj.wx          weighting factors for x (rows: trials, columns: levels)
%         est.traj.dax         volatility prediction errors in x (rows: trials, columns: levels)
%         est.traj.mua         mu_alpha (rows: trials, columns: levels)
%         est.traj.saa         sigma_alpha (rows: trials, columns: levels)
%         est.traj.muahat      prediction of mu_alpha (rows: trials, columns: levels)
%         est.traj.saahat      precisions of predictions of alpha (rows: trials, columns: levels)
%         est.traj.wa          weighting factors for alpha (rows: trials, columns: levels)
%         est.traj.daa         volatility prediction errors in alpha (rows: trials, columns: levels)
%         est.traj.dau         input prediction error
%
%
%
% Tips:
% - If you get an error saying that the prior means are in a region where model assumptions are
%   violated, lower the prior means of the omegas, starting with the highest level and proceeding
%   downwards.
%
% - Alternatives are lowering the prior means of the kappas, if they are not fixed, or adjusting
%   the values of the kappas or omegas, if any of them are fixed.
%
% --------------------------------------------------------------------------------------------------
% Copyright (C) 2013-2020 Christoph Mathys, TNU, UZH & ETHZ
%
% This file is part of the HGF toolbox, which is released under the terms of the GNU General Public
% Licence (GPL), version 3. You can redistribute it and/or modify it under the terms of the GPL
% (either version 3 or, at your option, any later version). For further details, see the file
% COPYING or <http://www.gnu.org/licenses/>.


% Config structure
c = struct;

% Model name
c.model = 'ehgf_jget';

% Number of levels (minimum: 2)
c.n_levels = 2;

% Sufficient statistics of Gaussian parameter priors

% PLACEHOLDER VALUES
% It is often convenient to set some priors to values
% derived from the inputs. This can be achieved by
% using placeholder values. The available placeholders
% are:
%
% 99991   Value of the first input
%         Usually a good choice for mux_0mu(1)
% 99992   Variance of the first 20 inputs
%         Usually a good choice for mux_0sa(1)
% 99993   Log-variance of the first 20 inputs
%         Usually a good choice for logsax_0mu(1) and mua_0mu(1)
% 99994   Log-variance of the first 20 inputs minus two
%         Usually a good choice for omxmu(1)

% Initial mus and sigmas
% Format: row vectors of length n_levels
% For all but the first level, this is usually best
% kept fixed to 1 (determines origin on x_i-scale).
c.mux_0mu = [86, 1];
c.mux_0sa = [10, 0];

% c.mux_0sa = [10, 1];

c.logsax_0mu = [      log(0.1),  log(0.01)];
c.logsax_0sa = [      0,   0];

c.mua_0mu = [6, 0];
c.mua_0sa = [10, 0];
% c.mua_0sa = [   10, 0];

c.logsaa_0mu = [log(0.1), log(0.01)];
c.logsaa_0sa = [     0,   0];

% Kappas
% Format: row vector of length n_levels-1 (except kappa_u: scalar)
% This should be fixed (preferably to 1) if the observation model
% does not use mu_i+1 (kappa then determines the scaling of x_i+1).
c.logkaumu = log(1);
c.logkausa = 0;

c.logkaxmu = [log(1)];
c.logkaxsa = [     1];

c.logkaamu = [log(1)];
c.logkaasa = [  1];

% Omegas
% Format: row vector of length n_levels (except omega_u: scalar)
c.omumu = 0;
c.omusa = 0;

c.omxmu = [  3,  -8]; %5 -3
c.omxsa = [  16,  0];

c.omamu = [  3,   -12]; % 2 0
c.omasa = [  0,  0];

% Gather prior settings in vectors
c.priormus = [
    c.mux_0mu,...
    c.logsax_0mu,...
    c.mua_0mu,...
    c.logsaa_0mu,...
    c.logkaumu,...
    c.logkaxmu,...
    c.logkaamu,...
    c.omumu,...
    c.omxmu,...
    c.omamu,...
         ];

c.priorsas = [
    c.mux_0sa,...
    c.logsax_0sa,...
    c.mua_0sa,...
    c.logsaa_0sa,...
    c.logkausa,...
    c.logkaxsa,...
    c.logkaasa,...
    c.omusa,...
    c.omxsa,...
    c.omasa,...
         ];

% Check whether we have the right number of priors
expectedLength = 8*c.n_levels;
if length([c.priormus, c.priorsas]) ~= 2*expectedLength
    error('tapas:hgf:PriorDefNotMatchingLevels', 'Prior definition does not match number of levels.')
end

% Model function handle
c.prc_fun = @tapas_ehgf_jget;

% Handle to function that transforms perceptual parameters to their native space
% from the space they are estimated in
c.transp_prc_fun = @tapas_ehgf_jget_transp;

end
