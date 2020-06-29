try_loc = function(file_loc)
	{
		tryCatch(source(file_loc),
			 error = function(e){
				 cat(paste("[ERROR] : \n\tCould not load file '.", file_loc, "'\n\n\tPlease fix any script issues",
            "and make sure you run this script from the repository root directory.\n\n"))
				 e
			})

      return(file_loc)
}

add_totals = function(run, totals)
{
  regions = run$dynamics[, unique(population)];
  
  # totals by age
  totals0 = run$dynamics[, .(total = sum(value)), by = .(scenario, run, compartment, group)];
  return (rbind(totals, totals0))
}

add_dynamics = function(run, dynamics, iv)
{
  regions = run$dynamics[, unique(population)];
  
  interv = data.table(scenario = run$dynamics$scenario[1], run = run$dynamics$run[1], t = unique(run$dynamics$t), 
                      compartment = "trace_school", value = unlist(iv$trace_school));
  
  if (!is.null(iv$trace_intervention)) {
    interv = rbind(interv,
                   data.table(scenario = run$dynamics$scenario[1], run = run$dynamics$run[1], t = unique(run$dynamics$t), 
                              compartment = "trace_intervention", value = unlist(iv$trace_intervention)));
  } else {
    interv = rbind(interv,
                   data.table(scenario = run$dynamics$scenario[1], run = run$dynamics$run[1], t = unique(run$dynamics$t), 
                              compartment = "trace_intervention", value = 1));
  }
  
  csvlines = NULL;
  
  if (nchar(run$csv[[1]]) > 0) {
    csvlines = fread(run$csv[[1]], header = F);
    csvlines = cbind(run$dynamics$scenario[1], run$dynamics$run[1], csvlines);
    names(csvlines) = c("scenario", "run", "t", "compartment", "value");
    csvlines = unique(csvlines);
  }
  # time courses
  return (rbind(dynamics, interv, csvlines
  ))
}

rebuild_cpp_libraries = function(cm_path)
{
  # Build C++ code
  packageStartupMessage("Attaching C++ code...")
  sourceCpp(file.path(cm_path, "model_v1", "corona.cpp"), cacheDir = file.path(cm_path, "covidm", "build"))
}