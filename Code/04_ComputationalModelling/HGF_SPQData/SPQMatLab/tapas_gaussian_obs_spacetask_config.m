function c = tapas_gaussian_obs_spacetask_config
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Contains the configuration for the Gaussian noise observation model for continuous responses
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The Gaussian noise observation model assumes that responses have a Gaussian distribution around
% the inferred mean of the relevant state. The only parameter of the model is the noise variance
% (NOT standard deviation) zeta.
%
% --------------------------------------------------------------------------------------------------
% Copyright (C) 2012-2013 Christoph Mathys, TNU, UZH & ETHZ
%
% This file is part of the HGF toolbox, which is released under the terms of the GNU General Public
% Licence (GPL), version 3. You can redistribute it and/or modify it under the terms of the GPL
% (either version 3 or, at your option, any later version). For further details, see the file
% COPYING or <http://www.gnu.org/licenses/>.


% Config structure
c = struct;

% Model name and noise effect
c.model = 'tapas_gaussian_obs_spacetask';
c.dyn_noise = 0; 

% Sufficient statistics of Gaussian parameter priors
%
% Zeta
% if c.dyn_noise == 0

c.logbe1mu = log(1); % for standard model with (free) noise parameter, for dynamic: log(1)
c.logbe1sa = 4^2;

c.logzemmu = 10; % for standard model with (free) noise parameter, for dynamic: log(1)
c.logzemsa = 4^2;

c.logzesmu = log(0.01); % for standard model with (free) noise parameter, for dynamic: log(1)
c.logzessa = 4^2;


% elseif c.dyn_noise == 1
% c.logzemu = tapas_logit(0.5, 1); % for standard model with (free) noise parameter, for dynamic: log(1)
% c.logzesa = 1;
% end

% Gather prior settings in vectors
c.priormus = [
    c.logbe1mu,...
    c.logzemmu,...
    c.logzesmu,...
         ];

c.priorsas = [
    c.logbe1sa,...
    c.logzemsa,...
    c.logzessa,...
              ];

% Model filehandle
c.obs_fun = @tapas_gaussian_obs_spacetask;

% Handle to function that transforms observation parameters to their native space
% from the space they are estimated in
c.transp_obs_fun = @tapas_gaussian_obs_spacetask_transp;

return;
