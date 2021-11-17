import os
import random

if __name__ == '__main__':
    var_min, var_max = -100, 100

    testcases_path = 'testcase'
    sols_dir = 'sols'
    eval_c = 'eval.c'
    
    sol_path = os.path.join(sols_dir, 'sols')
    sol_exe = 'temp_sol'
    sol_gcc_cmd = f'gcc -std=c11 -o {sol_exe} {os.path.join(sols_dir, eval_c)}'

    # Main.c
    # main_c = 'optimized.c'
    main_c = 'main.c'
    main_exe = 'main'
    main_output = 'out.txt'
    main_gcc_cmd = f'gcc -std=c11 -o {main_exe} {main_c}'
    os.system(main_gcc_cmd)

    # ASMC
    asmc = 'ASMC.cpp'
    asmc_dir = 'AssemblyCompiler'
    asmc_path = os.path.join(asmc_dir, asmc)
    asmc_exe = 'ASMC'
    asmc_gcc_cmd = f'g++ {asmc_path} -o {asmc_exe}'
    os.system(asmc_gcc_cmd)

    file_head_list = []
    for (dirpath, dirnames, filenames) in os.walk(testcases_path):
        for filename in filenames:
            # Varibles
            x, y, z = random.randint(var_min, var_max), random.randint(var_min, var_max), random.randint(var_min, var_max)
            # x, y, z = 2, 3, 5

            file_head = filename.split('.')[0]
            file_head_list.append(file_head)

            tc_in = f"{os.path.join(testcases_path, filename)}"
            sol_c = f'{os.path.join(sol_path, file_head)}.c'
            print("\n=================")
            if os.path.isfile(sol_c):
                # Compile solution
                print(f"->Compiling {file_head}")
                sol_compile_cmd = f"{sol_gcc_cmd} {sol_c}"
                print(sol_compile_cmd)
                os.system(sol_compile_cmd)

                # Init varibles
                print(f"Init:\nX = {x} / Y = {y} / Z = {z}\n")

                # Execute executable
                print("Expected:")
                os.system(f"./{sol_exe} {x} {y} {z}")

                # Execute main.c
                main_c_cmd = f"./{main_exe} {x} {y} {z} < {tc_in} > {main_output}"
                print(os.popen(main_c_cmd).read())

                # ASMC
                asmc_cmd = f"./{asmc_exe} {x} {y} {z} < {main_output}"

                print("Result: ")
                os.system(asmc_cmd)

                # Remove
                os.system(f"rm {sol_exe} {main_output}")
            else:
                print(f"->Skip {file_head}")
            print("=================\n")
