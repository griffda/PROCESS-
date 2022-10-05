from process.io.process_config import RunProcessConfig

config = RunProcessConfig("run_process.conf")

config.setup()

Path = "~/process/tests/regression/scenarios/baseline_jan_2017/IN.DAT"

input_path = Path(config.wdir) / "IN.DAT"

config.run_process(

    input_path

)